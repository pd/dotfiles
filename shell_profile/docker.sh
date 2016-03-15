if which docker-machine >/dev/null 2>&1; then
  if [[ -z "${DOCKER_HOST}" ]]; then
    export DOCKER_TLS_VERIFY="1"
    export DOCKER_HOST="tcp://192.168.45.128:2376"
    export DOCKER_CERT_PATH="/Users/khargraves/.docker/machine/machines/dev"
    export DOCKER_MACHINE_NAME="dev"
  fi
fi
