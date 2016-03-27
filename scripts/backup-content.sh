#!/bin/sh

#
# Make a resumable local backup of our content using `rsync`:
#

rsync -avz \
  -e "ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null" \
  --progress \
  root@v1.zoomhub.net:/mnt/datavolume/zoomhub/data/content-by-id/ \
  ~/zoomhub-backup/content-by-id/
