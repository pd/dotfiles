if which boot2docker >/dev/null 2>&1; then
  export DOCKER_HOST=tcp://127.0.0.1:2376
  export DOCKER_CERT_PATH=$HOME/.boot2docker/certs/boot2docker-vm
  export DOCKER_TLS_VERIFY=1
fi
