#!/bin/bash

set -eo pipefail

function run() {
  rclone purge \
    zoomhub-rackspace-cloudfiles:cache \
    --progress \
    --fast-list \
    --retries 4 \
    --retries-sleep=2s \
    --transfers=90 \
    --checkers=90 #--dry-run
}


n=0
until [ "$n" -ge 50 ]
do
   echo ''
   echo '-----------------------------------------------------------------------'
   echo "Attempt: $((n + 1))"
   echo '-----------------------------------------------------------------------'
   echo ''
   run && break
   n=$((n+1))
   sleep 5
done
