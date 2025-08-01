#!/usr/bin/env bash

set -euo pipefail

ACTION="${ACTION:-symlink}"

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
  music) _filebot "${DB:-ID3}" /media/sorted/Music "$path" "{any{albumArtist}{artist}}/{album} ({y})/{any{albumArtist}{artist}} - {album} ({y}) - {pi.pad(2)}. {t}" ;;
  *)     usage;;
  esac
}

[[ "$#" -eq 2 ]] || usage
main "$@"
