#!/bin/bash

set -eo pipefail

rclone delete \
  zoomhub-rackspace-cloudfiles:cache \
  --progress \
  --fast-list \
  --retries 8 \
  --retries-sleep=2s # --dry-run
