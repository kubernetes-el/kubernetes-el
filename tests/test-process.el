;;; test-process.el --- Tests for process management. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load-file "./tests/undercover-init.el")

(require 'kubernetes-process)

(describe "kubernetes--val-from-arg-list"
  (it "recognizes conjoined flag-value pairs"
    (expect
     (kubernetes--val-from-arg-list '("--foo=111") 'foo)
     :to-equal
     "111"))
  (it "recognizes separated flag-value pairs"
    (expect (kubernetes--val-from-arg-list '("--foo" "111") 'foo) :to-equal
            "111"))
  (it "returns nil on nil arg-list"
    (expect (kubernetes--val-from-arg-list nil 'foo) :to-equal nil))
  (it "returns nil on flag not found"
    (expect (kubernetes--val-from-arg-list '("--foo" "bar") 'baz) :to-equal nil)))

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

;;; test-process.el ends here
