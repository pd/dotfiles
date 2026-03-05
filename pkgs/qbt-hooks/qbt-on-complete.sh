DATA_PATH="${1:-}"
CATEGORY="${2:-}"

case "$CATEGORY" in
  ptparchive|opsbetter) exit 0 ;;
esac

curl -s -X POST "http://torrent.home/_hooks/completed" \
  --data-urlencode "path=$DATA_PATH" \
  --data-urlencode "tracker=$CATEGORY" \
  --data-urlencode "role="
