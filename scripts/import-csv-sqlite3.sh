#!/bin/bash

cd ~/zoom-it-data-git

rm -rf output
mkdir -p output
csplit -n 4 -f './output/split-ContentInfo-' -k ~/zoom-it-data-git/ContentInfo.txt '/^#Attributes.*/' '{9999}'

# Print unique header lines:
cat output/* | grep '^#Attributes' | sort | uniq
# > #Attributes PartitionKey  RowKey  Timestamp AbuseLevel  AttributionLink AttributionText Blob  Error Id  Mime  NumAbuseReports Progress  Size  Stage Title Type  Url Version
# > #Attributes PartitionKey  RowKey  Timestamp AbuseLevel  Blob  Error Id  Mime  NumAbuseReports Progress  Size  Stage Type  Url Version AttributionLink AttributionText Title

cd - > /dev/null
