if which boot2docker 2>&1 >/dev/null; then
  if [[ $(boot2docker status) == "running" ]]; then
    eval $(boot2docker shellinit 2>/dev/null)
  fi
fi
