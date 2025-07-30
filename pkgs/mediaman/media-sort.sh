#!/usr/bin/env bash

set -euo pipefail

usage() {
  echo "$0 [tv|movie|music] [path]" >&2
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
    --action symlink \
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
  tv)    _filebot "TheMovieDB::TV" /media/sorted/TV "$path" ;;
  movie) _filebot "TheMovieDB" /media/sorted/Movies "$path" ;;
  music) _filebot "ID3" /media/sorted/Music "$path" "{artist}/{album} ({y})/{artist} - {album} ({y}) - {pi.pad(2)}. {t}" ;;
  *)     usage;;
  esac
}

[[ "$#" -eq 2 ]] || usage
main "$@"
