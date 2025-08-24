# -*- mode: sh; sh-shell: bash -*-

set -euo pipefail

retag() {
  local dir="$1"; shift
  local mods=("$@")
  local flags=()

  for mod in "${mods[@]}"; do
    tag="$(echo "$mod" | cut -d= -f1)"
    value="$(echo "$mod" | cut -d= -f2-)"

    flags+=("--remove-tag=$tag")
    if [[ -n "$value" ]]; then
      flags+=("--set-tag=$tag=\"$value\"")
    fi
  done

  metaflac \
    --block-type VORBIS_COMMENT \
    "${flags[@]}" \
    "$tmpdir"/*.flac
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

  echo $link $src
  if [[ ! -d "$src" ]]; then
    echo "Source dir detection failed: $src" >&2
    exit 1
  fi

  rm -r "$dir"
  cp "$src/"*.flac "$tmpdir"
  retag "$tmpdir" "${mods[@]}"

  ACTION=move media-sort music "$tmpdir"
}

tmpdir="$(mktemp -d)"
trap 'rm -rf "$tmpdir"' EXIT
main "$tmpdir" "$@"
