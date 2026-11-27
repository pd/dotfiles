# -*- mode: sh; sh-shell: bash -*-

set -euo pipefail

NAS_HOST="${NAS_HOST:-nas}"
LOCAL_ROOT="${LOCAL_ROOT:-/media}"
NAS_ROOT="${NAS_ROOT:-/volume1/media}"

retag() {
  local dir="$1"; shift
  local mods=("$@")
  local flags=()

  for mod in "${mods[@]}"; do
    tag="$(echo "$mod" | cut -d= -f1)"
    value="$(echo "$mod" | cut -d= -f2-)"

    flags+=("--remove-tag=$tag")
    if [[ -n "$value" ]]; then
      flags+=("--set-tag=$tag=$value")
    fi
  done

  metaflac \
    --block-type VORBIS_COMMENT \
    "${flags[@]}" \
    "$dir"/*.flac
}

to_nas() {
  local path="$1"
  if [[ "$path" != "$LOCAL_ROOT"/* ]]; then
    echo "Not under $LOCAL_ROOT: $path" >&2
    exit 1
  fi
  echo "$NAS_ROOT/${path#"$LOCAL_ROOT"/}"
}

reflink_copy() {
  # paths over stdin so i don't have to juggle quoting quotes etc
  printf '%s\n%s\n' "$(to_nas "$1")" "$(to_nas "$2")" |
    ssh -o BatchMode=yes -o LogLevel=ERROR "$NAS_HOST" \
      'IFS= read -r src && IFS= read -r dest && cp --reflink=always -t "$dest" -- "$src"/*.flac'
}

main() {
  local tmpdir="$1"; shift
  local dir="$1"; shift
  local mods=("$@")

  if [[ ! -d "$dir" ]]; then
    echo "Not a dir: $dir" >&2
    exit 1
  fi

  local link
  link="$(ls -1 "$dir"/*.flac | head -1)"
  if [[ ! -f "$link" ]]; then
    echo "No FLAC in dir: $dir" >&2
    exit 1
  fi

  local src
  src="$(dirname "$(readlink -f "$link")")"

  if [[ ! -d "$src" ]]; then
    echo "Source dir detection failed: $src" >&2
    exit 1
  fi

  reflink_copy "$src" "$tmpdir"
  retag "$tmpdir" "${mods[@]}"
  rm -r "$dir"

  ACTION=move media-sort music "$tmpdir"
}

tmpdir="$(mktemp -d -p "$LOCAL_ROOT" .retag.XXXXXX)"
trap 'rm -rf "$tmpdir"' EXIT
main "$tmpdir" "$@"
