#!/usr/bin/env bash

set -euo pipefail
IFS=$'\n'

COMPLETED_DIR=${COMPLETED_DIR:-"/srv/transmission/done"}
KODI_ROOT=${KODI_ROOT:-"/srv/kodi"}
KODI_MOVIES_DIR=${KODI_MOVIES_DIR:-"${KODI_ROOT}/Movies"}
KODI_TV_DIR=${KODI_TV_DIR:-"${KODI_ROOT}/TV"}

tracker() {
  local id="$1"
  local tracker="$(transmission-remote --torrent "$id" --info-trackers | awk '/Tracker 0/ { print $3 }')"

  case "$tracker" in
  *passthepopcorn*) echo "ptp";;
  *landof.tv*) echo "btn";;
  *) echo "unknown";;
  esac
}

torrent_files() {
  local id="$1"
  transmission-remote --torrent "$id" --files | \
    sed -n '/[0-9]: /p' | \
    awk '{for(i=7; i<=NF; i++) printf "%s ", $i; print ""}' | \
    sed 's/ *$//'
}

stage_movie() {
  local id="$1"
  local path

  torrent_files "$id" | \
    while read path; do
      mkdir -p "${KODI_MOVIES_DIR}/$(dirname "$path")"
      [[ ! -e "${KODI_MOVIES_DIR}/${path}" ]] && ln -s "${COMPLETED_DIR}/${path}" "${KODI_MOVIES_DIR}/${path}"
    done
}

# IN (series): The.Expanse.S05.720p.REPACK.AMZN.WEBRip.DDP5.1.x264-NTb/The.Expanse.S05E...mkv
# IN (1ep):    The.Expanse.S05E03.....mkv
# OUT:         The Expanse
extract_series() {
  basename "$(echo "$1" | sed -E 's/^(.+)\.S[0-9]+E?[0-9]*\..+$/\1/')" | sed 's/\./ /g'
}

# OUT: 01
extract_season() {
  echo "$1" | sed -E 's/^.+\.S([0-9]+)E[0-9]+.+$/\1/i'| tr 'a-z' 'A-Z'
}

# OUT: S01E02
extract_episode() {
  echo "$1" | sed -E 's/^.+\.(S[0-9]+E[0-9]+).+$/\1/i' | tr 'a-z' 'A-Z'
}

# OUT: mkv
extract_filetype() {
  echo "$1" | sed -E 's/^.+\.([^.]+)$/\1/' | tr 'A-Z' 'a-z'
}

stage_tv() {
  local id="$1"
  local path
  local series season episode ext
  local season_dir episode_path

  torrent_files "$id" | \
    while read path; do
      series="$(extract_series "$path")"
      season="$(extract_season "$path")"
      episode="$(extract_episode "$path")"
      ext="$(extract_filetype "$path")"

      season_dir="${KODI_TV_DIR}/${series}/Season ${season}"
      episode_path="${season_dir}/${episode}.${ext}"

      mkdir -p "$season_dir"
      [[ ! -e "$episode_path" ]] && ln -s "${COMPLETED_DIR}/$path" "$episode_path"
    done
}

relocate() {
  local id="$1"
  local tracker="$(tracker "$id")"

  if [[ "$tracker" == "ptp" ]]; then
    stage_movie "$id"
  elif [[ "$tracker" == "btn" ]]; then
    stage_tv "$id"
  fi
}

relocate "${TR_TORRENT_ID:-$1}"
kodi-send -a "UpdateLibrary('video')"
