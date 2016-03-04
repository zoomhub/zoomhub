#!/bin/sh

#
# Import missing
#

MISSING_BY_ID='/root/zoomit-missing-data/output'
CONTENT_BY_ID='/mnt/datavolume/zoomhub/data/content-by-id'
CONTENT_BY_URL='/mnt/datavolume/zoomhub/data/content-by-url'
for target in $MISSING_BY_ID/*.json ;
do
  contentFilename=$(basename $target)
  content=$(cat "$target")
  contentID=$(printf '%s' "$content" | json 'id')
  contentURL=$(printf '%s' "$content" | json url)
  targetHash=$(printf '%s' "$contentURL" | sha256sum | tr -d '[:space:]-')
  linkName="$CONTENT_BY_URL/$targetHash.json"
  idTarget="$CONTENT_BY_ID/$contentFilename"

  echo "Processing: $contentID ($idTarget)"
  cp --no-clobber "$target" "$idTarget"
  ln "$idTarget" "$linkName"
done
