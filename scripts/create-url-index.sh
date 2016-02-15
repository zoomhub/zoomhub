#!/bin/sh

#
# Create URL --> ID index using hardlinks to avoid consuming inodes.
#

CONTENT_BY_ID='/mnt/datavolume/zoomhub/data/content-by-id'
CONTENT_BY_URL='/mnt/datavolume/zoomhub/data/content-by-url'
for target in $CONTENT_BY_ID/{{k..z},{A..Z}}*.json ;
do
  content=$(cat "$target")
  contentID=$(printf '%s' "$content" | json 'id')
  contentURL=$(printf '%s' "$content" | json url)
  targetHash=$(printf '%s' "$contentURL" | sha256sum | tr -d '[:space:]-')
  linkName="$CONTENT_BY_URL/$targetHash.json"
  echo "Processing: $contentID"
  ln $target $linkName
done
