# -*- mode: sh; sh-shell: bash -*-

set -euo pipefail

ACTION="${ACTION:-symlink}"

music_format=$(cat <<'EOF'
{
  def name = any{ albumArtist.replaceAll(/^(.+) \1$/){ it[1] } }{ artist }
  name = new File("/media/sorted/Music").list().find{ it.equalsIgnoreCase(name) } ?: name

  def discs = any{ media.Part_Position_Total.toInteger() }{ 1 }
  def disc = discs > 1 ? "Disc ${media.Part_Position}/" : ""
  def year = any{ " (${y})" }{ null } ?: ""

  "${name}/${album}${year}/${disc}${name} - ${album}${year} - ${pi.pad(2)}. ${t}"
}
EOF
)

usage() {
  echo "$0 [tv|movie|music] [path]" >&2
  echo "  \$ACTION: --action flag [move, symlink, test]; default: symlink"
  exit 1
}

_filebot() {
  local db="$1"
  local dest="$2"
  local path="$3"

  local format="${4:-}"
  if [[ "$format" == "" ]]; then
    format="{jellyfin.tail}"
  fi

  filebot \
    -rename \
    -non-strict \
    -no-xattr \
    --action "$ACTION" \
    --conflict skip \
    --format "$format" \
    --output "$dest" \
    --db "$db" \
    -r "$path"
}

main() {
  local kind="$1"
  local path="$2"

  case "$kind" in
  tv)    _filebot "${DB:-TheMovieDB::TV}" /media/sorted/TV "$path" ;;
  movie) _filebot "${DB:-TheMovieDB}" /media/sorted/Movies "$path" ;;
  music) _filebot "${DB:-ID3}" /media/sorted/Music "$path" "$music_format" ;;
  *)     usage;;
  esac
}

[[ "$#" -eq 2 ]] || usage
main "$@"
