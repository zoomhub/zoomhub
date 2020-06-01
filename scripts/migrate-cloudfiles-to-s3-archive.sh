#!/bin/bash

set -eo pipefail

rclone copy \
    zoomhub-rackspace-cloudfiles:archive \
    zoomhub-aws-s3:zoomhub-archive \
    --progress \
    --fast-list \
    --transfers=32 \
    --checkers=32 #--dry-run
