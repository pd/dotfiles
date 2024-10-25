#!/usr/bin/env bash
set -euo pipefail

(whoami &&
   pwd &&
   env &&
   echo "-----------------") >> /var/lib/transmission/torrent-completed.log
