#!/bin/sh

#
# Make a resumable local backup of our content using `rsync`:
#

rsync -avz \
  -e "ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null" \
  --progress \
  root@104.130.230.137:/mnt/datavolume/zoomhub/data/content-by-id/ \
  ~/zoomhub-backup
