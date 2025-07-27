#!/usr/bin/env bash

set -euo pipefail

valid_link() {
  local path="$1"
  local target="$(readlink -f "$path")"
  test -f "$target"
}

IFS=$'\n'
while read path; do
  if ! valid_link "$path"; then
    echo "Pruning broken link: $path"
    rm -f "$path"
  fi
done <<< "$(find /media/sorted -type l)"
