curl -sf \
  -u "$NTFY_USER:$NTFY_PASS" \
  -d alive \
  "https://ntfy.krh.me/heartbeat-$HOSTNAME"
