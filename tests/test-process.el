;;; test-process.el --- Tests for process management. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load-file "./tests/undercover-init.el")

(require 'kubernetes-process)

(describe "kubernetes--request-option"
  (describe "when request throws an error"
    (before-each
      (spy-on 'request
              :and-call-fake (lambda (&rest _)
                               (make-request-response
                                :status-code 404
                                :error-thrown (error . ("bar"))))))
    (it "throws an error"
      (expect (kubernetes--request-option "foo") :to-throw 'error)))
  (describe "when request returns successfully"
    (before-each
      (spy-on 'request
              :and-return-value (make-request-response
                                 :status-code 200
                                 :error-thrown nil)))
    (it "returns the response"
      (expect (kubernetes--request-option "foo") :to-be-truthy))))

(describe "Process ledger"
  :var (ledger)
  (before-each
    (setq ledger (kubernetes--process-ledger))
    (spy-on 'process-live-p
            :and-call-fake
            (lambda (proc)
              (cond
               ((eq proc :fake-proc-live) t)
               ((eq proc :fake-proc-dead) nil)))))

  (describe "kill-proxy-process"
    (before-each
      (spy-on 'kubernetes-process-kill-quietly))
    (it "does nothing if no proxy process present"
      (kill-proxy-process (kubernetes--process-ledger :proxy nil))
      (expect 'kubernetes-process-kill-quietly :not :to-have-been-called))
    (it "terminates and wipes existing proxy process"
      (setq ledger (kubernetes--process-ledger
                    :proxy (kubernetes--ported-process-record
                            :process :existing-process
                            :address "localhost"
                            :port 9999)))
      (kill-proxy-process ledger)
      (expect 'kubernetes-process-kill-quietly :to-have-been-called)
      (expect (oref ledger proxy) :to-equal nil)))

  (describe "proxy-active-p"
    (it "returns nil if there is no record"
      (expect (proxy-active-p (kubernetes--process-ledger :proxy nil))
              :to-equal
              nil))
    (it "returns nil if record has nil process"
      (expect (proxy-active-p (kubernetes--process-ledger
                               :proxy (kubernetes--ported-process-record
                                       :process nil)))
              :to-equal
              nil))
    (it "returns nil if record's process is not alive"
      (spy-on 'process-live-p :and-return-value nil)
      (expect (proxy-active-p (kubernetes--process-ledger
                               :proxy (kubernetes--ported-process-record
                                       :process :fake-process)))
              :to-equal
              nil)
      (expect 'process-live-p :to-have-been-called))
    (it "returns t if record's process is alive"
      (spy-on 'process-live-p :and-return-value t)
      (expect (proxy-active-p (kubernetes--process-ledger
                               :proxy (kubernetes--ported-process-record
                                       :process :fake-process)))
              :to-equal
              t)
      (expect 'process-live-p :to-have-been-called)))

  (describe "get-proxy-process"
    (before-each
      (spy-on 'kill-proxy-process)
      (spy-on 'proxy-ready-p :and-return-value t)
      (spy-on 'kubernetes-kubectl :and-return-value :new-kubectl-proc)
      (spy-on 'sleep-for))
    (describe "when proxy present in ledger"
      (describe "when port specified in arguments"
        :var (args)
        (describe "when argument port doesn't match port of existing proxy"
          (before-each
            (setq args '("--port=1111"))
            (setq ledger (kubernetes--process-ledger
                          :proxy (kubernetes--ported-process-record
                                  :port 9999))))
          (it "destroys and creates new proxy process"
            (get-proxy-process ledger args)
            (expect 'kill-proxy-process :to-have-been-called)
            (expect (oref (oref ledger proxy) port) :to-equal 1111)))
        (describe "when argument port matches port of existing proxy"
          (before-each
            (setq args '("--port=1111"))
            (setq ledger (kubernetes--process-ledger
                          :proxy (kubernetes--ported-process-record
                                  :process :fake-process
                                  :port 1111))))
          (it "returns the existing proxy process"
            (expect (get-proxy-process ledger args) :to-equal :fake-process)
            (expect 'kill-proxy-process :not :to-have-been-called)
            (expect (oref (oref ledger proxy) port) :to-equal 1111))))
      (describe "when no port specified in arguments"
        (before-each
          (setq ledger (kubernetes--process-ledger
                        :proxy (kubernetes--ported-process-record
                                :process :fake-process))))
        (it "returns the existing proxy process"
          (expect (get-proxy-process ledger '("--something=else"))
                  :to-equal
                  :fake-process))))
    (describe "when proxy not present in ledger"
      (before-each
        (setq ledger (kubernetes--process-ledger :proxy nil)))
      (describe "when no port specified in arguments"
        (it "creates new proxy process with default port"
          (let ((kubernetes-default-proxy-port 1234))
            (expect (get-proxy-process ledger nil) :to-equal :new-kubectl-proc)
            (expect (oref (oref ledger proxy) port) :to-equal 1234))))
      (describe "when port specified in arguments"
        (it "creates new proxy process with that port"
          (get-proxy-process ledger '("--port" "1111"))
          (expect (oref (oref ledger proxy) port) :to-equal 1111)))))

  (describe "process lookup"
    (it "returns the process for resources"
      (setq ledger (kubernetes--process-ledger
                    :poll-processes '((pods . t)
                                      (deployments . nil))))
      (expect (get-process-for-resource ledger 'pods) :to-equal t)
      (expect (get-process-for-resource ledger 'deployments) :to-equal nil))

    (it "returns nil for resources with no process"
      (expect (get-process-for-resource ledger 'pods) :to-equal nil)))

  (describe "process setting"
    (describe "with empty processes list"
      (before-each
        (spy-on 'kubernetes-process-kill-quietly))
      (it "assigns successfully"
        (setq ledger (kubernetes--process-ledger))
        (set-process-for-resource ledger 'pods :fake-proc-live))

      (it "assigns, unassigns, then re-assigns successfully"
        (setq ledger (kubernetes--process-ledger))
        (set-process-for-resource ledger 'pods :fake-proc-live)
        (release-process-for-resource ledger 'pods)
        (set-process-for-resource ledger 'pods :fake-proc-dead)
        (expect (get-process-for-resource ledger 'pods) :to-equal :fake-proc-dead)))

    (describe "with existing processes"
      :var (ledger)
      (before-each
        (spy-on 'release-process-for-resource)
        (setq ledger (kubernetes--process-ledger
                      :poll-processes '((pods . nil)
                                        (services . :fake-proc-dead)
                                        (deployments . :fake-proc-live)))))

      (it "assigns successfully if no process exists"
        (expect (set-process-for-resource ledger 'pods :fake-proc-live) :not :to-throw 'error)
        (expect (get-process-for-resource ledger 'pods) :to-equal :fake-proc-live))

      (it "assigns successfully if no live process exists"
        (expect (set-process-for-resource ledger 'services :fake-proc-live) :not :to-throw 'error)
        (expect (get-process-for-resource ledger 'services) :to-equal :fake-proc-live))

      (it "assigns successfully if live process already exists, with force"
        (expect (set-process-for-resource ledger 'services :foo t) :not :to-throw 'error)
        (expect 'release-process-for-resource :to-have-been-called-with ledger 'services)
        (expect (get-process-for-resource ledger 'services) :to-equal :foo))

      (it "errors if a live process already exists"
        (expect (set-process-for-resource ledger 'deployments :fake-proc-live) :to-throw 'error))))

  (describe "process releasing"
    (before-each
      (spy-on 'kubernetes-process-kill-quietly))

    (it "can release all processes at once"
      (setq ledger (kubernetes--process-ledger
                    :poll-processes '((pods . :fake-proc-dead)
                                      (deployments . :fake-proc-live)
                                      (services . nil))))
      (expect (release-all ledger) :to-equal '(pods deployments))
      (expect (get-process-for-resource ledger 'pods) :to-equal nil)
      (expect (get-process-for-resource ledger 'deployments) :to-equal nil)
      (expect (get-process-for-resource ledger 'services) :to-equal nil))

    (it "releases existing live processes"
      (setq ledger (kubernetes--process-ledger
                    :poll-processes '((pods . :fake-proc-dead)
                                      (deployments . :fake-proc-live)
                                      (services . nil))))
      (release-process-for-resource ledger 'deployments)
      (expect 'kubernetes-process-kill-quietly :to-have-been-called-with
              :fake-proc-live)
      (expect (get-process-for-resource ledger 'deployments) :to-equal nil))

    (it "cleans up if existing process is already dead"
      (setq ledger (kubernetes--process-ledger
                    :poll-processes '((pods . :fake-proc-dead)
                                      (deployments . :fake-proc-live)
                                      (services . nil))))
      (release-process-for-resource ledger 'pods)
      (expect 'kubernetes-process-kill-quietly :not :to-have-been-called)
      (expect (get-process-for-resource ledger 'pods) :to-equal nil))

    (it "no-ops if no existing process"
      (setq ledger (kubernetes--process-ledger
                    :poll-processes '((pods . :fake-proc-dead)
                                      (deployments . :fake-proc-live)
                                      (services . nil))))
      (release-process-for-resource ledger 'services)
      (expect 'kubernetes-process-kill-quietly :not :to-have-been-called)))

  (describe "liveness checking"
    (it "returns the liveness of the process"

      (setq ledger (kubernetes--process-ledger
                    :poll-processes '((pods . :fake-proc-live)
                                      (secrets . :fake-proc-dead)
                                      (deployments . nil))))

      (expect (poll-process-live-p ledger 'pods) :to-be-truthy)
      (expect (poll-process-live-p ledger 'secrets) :not :to-be-truthy)
      (expect (poll-process-live-p ledger 'deployments) :not :to-be-truthy)
      (expect (poll-process-live-p ledger 'services) :not :to-be-truthy))))

(describe "kubernetes--ported-process-record"
  (describe "base-url"
    (it "interpolates the base URL for the process"
      (expect
       (base-url (kubernetes--ported-process-record
                  :address "foobar"
                  :port 1234))
       :to-equal "http://foobar:1234")))
  (describe "wait-on-endpoint"
    (describe "retrying"
      (before-each
        (spy-on 'sleep-for))
      (it "retries by default"
        (spy-on 'kubernetes--request-option :and-throw-error 'error)
        (expect (wait-on-endpoint (kubernetes--ported-process-record) "foo")
                :to-throw 'error)
        (expect 'sleep-for :to-have-been-called-times 10)
        (expect 'kubernetes--request-option :to-have-been-called-times 10))
      (it "retries by specified count and wait time"
        (spy-on 'kubernetes--request-option :and-throw-error 'error)
        (expect (wait-on-endpoint (kubernetes--ported-process-record) "foo" 15 3)
                :to-throw 'error)
        (expect 'sleep-for :to-have-been-called-times 15)
        (expect 'sleep-for :to-have-been-called-with 3)
        (expect 'kubernetes--request-option :to-have-been-called-times 15)))))

;;; test-process.el ends here
