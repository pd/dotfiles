curl \
  --silent \
  --fail \
  --request POST \
  --header "Authorization: Bearer $GATUS_HEARTBEAT_TOKEN" \
  "https://status.krh.me/api/v1/endpoints/heartbeat_$HOSTNAME/external?success=true"
