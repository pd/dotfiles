# -*- mode: sh; sh-shell: bash -*-

set -euo pipefail
metaflac --list --block-type VORBIS_COMMENT "$@"
