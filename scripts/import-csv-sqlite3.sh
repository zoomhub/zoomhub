#!/bin/bash

WORKING_DIRECTORY=$PWD
cd ~/zoom-it-data-git

echo '===> Initialize output directories'
rm -rf output
mkdir -p output/{group1,group2}

echo '===> Split files based on CSV (TSV) header line'
csplit -n 5 -f './output/split-ContentInfo-' -k ContentInfo.txt '/^#Attributes PartitionKey/' '{9999}' &> /dev/null

# Print unique header lines:
# cat output/split-ContentInfo-* | grep '^#Attributes' | sort | uniq
# > #Attributes PartitionKey  RowKey  Timestamp AbuseLevel  AttributionLink AttributionText Blob  Error Id  Mime  NumAbuseReports Progress  Size  Stage Title Type  Url Version
# > #Attributes PartitionKey  RowKey  Timestamp AbuseLevel  Blob  Error Id  Mime  NumAbuseReports Progress  Size  Stage Type  Url Version AttributionLink AttributionText Title

echo '===> Group files by CSV (TSV) header line type'
group1=$(grep --files-with-matches -R '^#Attributes PartitionKey\tRowKey\tTimestamp\tAbuseLevel\tAttributionLink\tAttributionText\tBlob\tError\tId\tMime\tNumAbuseReports\tProgress\tSize\tStage\tTitle\tType\tUrl\tVersion\r$' output)
group2=$(grep --files-with-matches -R '^#Attributes PartitionKey\tRowKey\tTimestamp\tAbuseLevel\tBlob\tError\tId\tMime\tNumAbuseReports\tProgress\tSize\tStage\tType\tUrl\tVersion\tAttributionLink\tAttributionText\tTitle\r$' output)
mv $group1 output/group1
mv $group2 output/group2

echo '===> Initialize database'
cat $WORKING_DIRECTORY/scripts/create-db.sql | sqlite3 output/zoomhub.sqlite3

echo '===> Import `ContentInfo` (group 1) into SQLite'
cat $(find output/group1 -type f | head -n 1) | head -n 1 > output/output-group1.csv
cat output/group1/* | grep --invert '^#Attributes PartitionKey' >> output/output-group1.csv
printf '.mode tab\n.import output/output-group1.csv ContentInfoGroup1' | sqlite3 output/zoomhub.sqlite3

echo '===> Import `ContentInfo` (group 2) into SQLite'
cat $(find output/group2 -type f | head -n 1) | head -n 1 > output/output-group2.csv
cat output/group2/* | grep --invert '^#Attributes PartitionKey' >> output/output-group2.csv
printf '.mode tab\n.import output/output-group2.csv ContentInfoGroup2' | sqlite3 output/zoomhub.sqlite3

echo '===> Import `ImageInfo` into SQLite'
cat ImageInfo.txt | head -n 1 > output/ImageInfo.csv
cat ImageInfo.txt | grep --invert '^#Attributes PartitionKey' >> output/ImageInfo.csv
printf '.mode tab\n.import output/ImageInfo.csv ImageInfo' | sqlite3 output/zoomhub.sqlite3

echo '===> Import `FlickrPhotoInfo` into SQLite'
cat FlickrPhotoInfo.txt | head -n 1 > output/FlickrPhotoInfo.csv
cat FlickrPhotoInfo.txt | grep --invert '^#Attributes PartitionKey' >> output/FlickrPhotoInfo.csv
printf '.mode tab\n.import output/FlickrPhotoInfo.csv FlickrPhotoInfo' | sqlite3 output/zoomhub.sqlite3

echo '===> Check consistency of `ContentInfoGroup1` table'
numMismatchingIdRowKeyGroup1=$(echo 'SELECT COUNT(*) FROM ContentInfoGroup1 WHERE RowKey <> Id;' | sqlite3 output/zoomhub.sqlite3)
if [[ "$numMismatchingIdRowKeyGroup1" != '0' ]] ; then
  echo "Found $numMismatchingIdRowKeyGroup1 rows in \`ContentInfoGroup1\` that have mismatching \`Id\` and \`RowKey\` columns."
  exit 1
fi

echo '===> Check consistency of `ContentInfoGroup2` table'
numMismatchingIdRowKeyGroup2=$(echo 'SELECT COUNT(*) FROM ContentInfoGroup2 WHERE RowKey <> Id;' | sqlite3 output/zoomhub.sqlite3)
if [[ "$numMismatchingIdRowKeyGroup2" != '0' ]] ; then
  echo "Found $numMismatchingIdRowKeyGroup2 rows in \`ContentInfoGroup2\` that have mismatching \`Id\` and \`RowKey\` columns."
  exit 1
fi

open output/zoomhub.sqlite3

cd - > /dev/null
