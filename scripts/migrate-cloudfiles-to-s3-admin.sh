#!/bin/bash

set -eo pipefail

rclone copy \
    zoomhub-rackspace-cloudfiles:admin \
    zoomhub-aws-s3:zoomhub-admin \
    --progress \
    --fast-list \
    --transfers=32 \
    --checkers=32 #--dry-run
