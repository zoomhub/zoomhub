#!/bin/sh

#
# Make a resumable local backup of our content using `rsync`:
#

mkdir -p ~/zoomhub-backup
rsync -avz \
  -e "ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null" \
  --progress \
  admin@zoomhub.net:/mnt/datavolume/zoomhub/data/zoomhub.sqlite3 \
  ~/zoomhub-backup/zoomhub-production-$(date -u +"%Y-%m-%dT%H-%M-%SZ").sqlite3
