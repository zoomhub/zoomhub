#!/bin/sh

#
# Make a resumable local backup of our content using `rsync`:
#

rsync -avz \
  -e "ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null" \
  --progress \
  admin@zoomhub.net:/mnt/datavolume/zoomhub/data/content.sqlite3 \
  ~/zoomhub-backup/content-$(date -u +"%Y-%m-%dT%H-%M-%SZ").sqlite3
