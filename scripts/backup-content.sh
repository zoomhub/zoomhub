#!/bin/sh

#
# Make a resumable local backup of our content using `rsync`:
#

TIMESTAMP=$(date -u +"%Y-%m-%dT%H-%M-%SZ")
echo "Timestamp: $TIMESTAMP"

mkdir -p ~/zoomhub-backup
ssh -t admin@zoomhub.net \
  "cd /mnt/datavolume/zoomhub/data/; sqlite3 zoomhub.sqlite3 \".backup 'zoomhub-backup-$TIMESTAMP.sqlite3'\""
rsync -avz \
  -e "ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null" \
  --progress \
  admin@zoomhub.net:"/mnt/datavolume/zoomhub/data/zoomhub-backup-$TIMESTAMP.sqlite3" \
  ~/zoomhub-backup/zoomhub-production-$TIMESTAMP.sqlite3
ssh -t admin@zoomhub.net \
  "cd /mnt/datavolume/zoomhub/data/; rm zoomhub-backup-$TIMESTAMP.sqlite3"
