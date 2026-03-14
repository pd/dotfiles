STATE_DIR=/var/lib/heart-monitor

_sub() {
  curl -s -u "sub:$NTFY_PASSWORD_SUB" "$@"
}

_pub() {
  curl -s -u "pub:$NTFY_PASSWORD_PUB" "$@"
}

previous_state() {
  local host="$1"
  cat "$STATE_DIR/$host" 2>/dev/null || echo ""
}

pronounce() {
  local host="$1"
  local state="$2"
  echo "$state" > "$STATE_DIR/$host"
}

is_alive() {
  local host="$1"
  _sub "https://ntfy.krh.me/heartbeat-${host}/json?since=10m&poll=1" |
    grep -q alive
}

announce() {
  local msg="$1"
  _pub -d "$msg" "https://ntfy.krh.me/lab"
}

for host in "$@"; do
  prev="$(previous_state "$host")"
  if is_alive "$host"; then
    pronounce "$host" alive
    if [[ "$prev" == "dead" ]]; then
      announce "$host back from dead"
    fi
  else
    pronounce "$host" dead
    if [[ "$prev" != "dead" ]]; then
      announce "$host heart not beating"
    fi
  fi
done
