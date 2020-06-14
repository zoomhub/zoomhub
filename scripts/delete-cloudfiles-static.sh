#!/bin/bash

set -eo pipefail

rclone delete \
  zoomhub-rackspace-cloudfiles:static \
  --progress \
  --fast-list \
  --retries 8 \
  --retries-sleep=2s \
  --transfers=64 \
  --checkers=64 #--dry-run
