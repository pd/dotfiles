#!/usr/bin/env bash

set -euo pipefail

usage() {
  echo "$0 [tv|movie] [path]" >&2
  exit 1
}

_filebot() {
  local db="$1"
  local dest="$2"
  local path="$3"

  filebot \
    -rename \
    -non-strict \
    -no-xattr \
    --action symlink \
    --conflict skip \
    --format '{jellyfin.tail}' \
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
  *)     usage;;
  esac
}

[[ "$#" -eq 2 ]] || usage
main "$@"
