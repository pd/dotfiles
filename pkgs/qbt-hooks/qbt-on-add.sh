HASH="${1:-}"
TRACKER="${2:-}"
CURRENT_CAT="${3:-}"
QBT_API="http://torrent.home/api/v2"
CATEGORIES="${QBT_CATEGORIES:-/run/secrets/qbt-categories}"

case "$CURRENT_CAT" in
  ptparchive|opsbetter) exit 0 ;;
esac

DOMAIN=$(echo "$TRACKER" | sed -E 's|^https?://([^/:]+).*|\1|')
CATEGORY=$(grep -F "$DOMAIN" "$CATEGORIES" 2>/dev/null | head -1 | cut -d= -f2)

echo "Categorizing $DOMAIN ($TRACKER) as $CATEGORY"
if [[ -n "$CATEGORY" ]]; then
  curl -s -X POST "$QBT_API/torrents/setCategory" \
    -d "hashes=$HASH&category=$CATEGORY"
fi
