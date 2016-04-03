#!/bin/bash

ROOT="$1"

if [[ ! -d "$ROOT" ]] ; then
  echo 'Please provide the path to the data directory: `./scripts/import-csv-sqlite3.sh DATA_DIRECTORY'
  exit 1
fi

echo '===> Initialize output directories'
rm -rf $ROOT/output
mkdir -p $ROOT/output/{group1,group2}

echo '===> Split files based on CSV (TSV) header line'
csplit -n 5 -f "$ROOT/output/split-ContentInfo-" -k $ROOT/ContentInfo.txt '/^#Attributes PartitionKey/' '{9999}' &> /dev/null

# Print unique header lines:
# cat output/split-ContentInfo-* | grep '^#Attributes' | sort | uniq
# > #Attributes PartitionKey  RowKey  Timestamp AbuseLevel  AttributionLink AttributionText Blob  Error Id  Mime  NumAbuseReports Progress  Size  Stage Title Type  Url Version
# > #Attributes PartitionKey  RowKey  Timestamp AbuseLevel  Blob  Error Id  Mime  NumAbuseReports Progress  Size  Stage Type  Url Version AttributionLink AttributionText Title

echo '===> Group files by CSV (TSV) header line type'
group1=$(grep --files-with-matches -R '^#Attributes PartitionKey\tRowKey\tTimestamp\tAbuseLevel\tAttributionLink\tAttributionText\tBlob\tError\tId\tMime\tNumAbuseReports\tProgress\tSize\tStage\tTitle\tType\tUrl\tVersion\r$' $ROOT/output)
group2=$(grep --files-with-matches -R '^#Attributes PartitionKey\tRowKey\tTimestamp\tAbuseLevel\tBlob\tError\tId\tMime\tNumAbuseReports\tProgress\tSize\tStage\tType\tUrl\tVersion\tAttributionLink\tAttributionText\tTitle\r$' $ROOT/output)
mv $group1 $ROOT/output/group1
mv $group2 $ROOT/output/group2

echo '===> Initialize database'
cat ./scripts/create-db.sql | sqlite3 $ROOT/output/zoomhub.sqlite3

echo '===> Import `ContentInfo` (group 1) into SQLite'
cat $(find $ROOT/output/group1 -type f | head -n 1) | head -n 1 > $ROOT/output/output-group1.csv
cat $ROOT/output/group1/* | grep --invert '^#Attributes PartitionKey' >> $ROOT/output/output-group1.csv
printf ".mode tab\n.import $ROOT/output/output-group1.csv ContentInfoGroup1" | sqlite3 $ROOT/output/zoomhub.sqlite3

echo '===> Import `ContentInfo` (group 2) into SQLite'
cat $(find $ROOT/output/group2 -type f | head -n 1) | head -n 1 > $ROOT/output/output-group2.csv
cat $ROOT/output/group2/* | grep --invert '^#Attributes PartitionKey' >> $ROOT/output/output-group2.csv
printf ".mode tab\n.import $ROOT/output/output-group2.csv ContentInfoGroup2" | sqlite3 $ROOT/output/zoomhub.sqlite3

echo '===> Import `ImageInfo` into SQLite'
cat $ROOT/ImageInfo.txt | head -n 1 > $ROOT/output/ImageInfo.csv
cat $ROOT/ImageInfo.txt | grep --invert '^#Attributes PartitionKey' >> $ROOT/output/ImageInfo.csv
printf ".mode tab\n.import $ROOT/output/ImageInfo.csv ImageInfo" | sqlite3 $ROOT/output/zoomhub.sqlite3

echo '===> Import `FlickrPhotoInfo` into SQLite'
cat $ROOT/FlickrPhotoInfo.txt | head -n 1 > $ROOT/output/FlickrPhotoInfo.csv
cat $ROOT/FlickrPhotoInfo.txt | grep --invert '^#Attributes PartitionKey' >> $ROOT/output/FlickrPhotoInfo.csv
printf ".mode tab\n.import $ROOT/output/FlickrPhotoInfo.csv FlickrPhotoInfo" | sqlite3 $ROOT/output/zoomhub.sqlite3

echo '===> Check consistency of `ContentInfoGroup1` table'
numMismatchingIdRowKeyGroup1=$(echo 'SELECT COUNT(*) FROM ContentInfoGroup1 WHERE RowKey <> Id;' | sqlite3 $ROOT/output/zoomhub.sqlite3)
if [[ "$numMismatchingIdRowKeyGroup1" != '0' ]] ; then
  echo "Found $numMismatchingIdRowKeyGroup1 rows in \`ContentInfoGroup1\` that have mismatching \`Id\` and \`RowKey\` columns."
  exit 1
fi

echo '===> Import legacy data into new schema'
cat ./scripts/migrate-legacy-schema.sql | sqlite3 $ROOT/output/zoomhub.sqlite3

echo '===> Check `content` table for completeness'
numContentRows=$(echo 'SELECT COUNT(*) FROM content;' | sqlite3 $ROOT/output/zoomhub.sqlite3)
if [[ "$numContentRows" != '1550351' ]] ; then
  echo "Found $numContentRows rows in \`content\` but expected 1550351."
  exit 1
fi

echo '===> Check `image` table for completeness'
numImageRows=$(echo 'SELECT COUNT(*) FROM image;' | sqlite3 $ROOT/output/zoomhub.sqlite3)
if [[ "$numImageRows" != '1430690' ]] ; then
  echo "Found $numImageRows rows in \`image\` but expected 1430690."
  exit 1
fi

echo '===> Check `flickr` table for completeness'
numFlickrRows=$(echo 'SELECT COUNT(*) FROM flickr;' | sqlite3 $ROOT/output/zoomhub.sqlite3)
if [[ "$numFlickrRows" != '21871' ]] ; then
  echo "Found $numFlickrRows rows in \`flickr\` but expected 21871."
  exit 1
fi

open $ROOT/output/zoomhub.sqlite3
