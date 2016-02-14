#!/bin/sh

#
# Before:
#   > df -i /mnt/datavolume/zoomhub/
#   Filesystem      Inodes   IUsed   IFree IUse% Mounted on
#   /dev/xvdb1     6553600 1192855 5360745   19% /mnt/datavolume
#

CONTENT_BY_ID='/mnt/datavolume/zoomhub/data/content-by-id'
CONTENT_BY_URL='/mnt/datavolume/zoomhub/data/content-by-url'
for target in $CONTENT_BY_ID/*.json ;
do
  content=$(cat "$target")
  contentID=$(printf '%s' "$content" | json 'id')
  contentURL=$(printf '%s' "$content" | json url)
  targetHash=$(printf '%s' "$contentURL" | sha256sum | tr -d '[:space:]-')
  linkName="$CONTENT_BY_URL/$targetHash.json"
  echo "Processing: $contentID"
  ln $target $linkName
done
