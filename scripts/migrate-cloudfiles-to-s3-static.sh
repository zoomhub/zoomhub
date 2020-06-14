#!/bin/bash

set -eo pipefail

rclone copy \
    zoomhub-rackspace-cloudfiles:static \
    zoomhub-aws-s3:static.zoomhub.net \
    --progress \
    --fast-list \
    --s3-acl='public-read'
