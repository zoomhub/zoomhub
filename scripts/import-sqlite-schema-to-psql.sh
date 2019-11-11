#!/bin/bash

set -eo pipefail

SQLITE_DB="$1"

if [ -z "$SQLITE_DB" ]; then
  echo "Missing 'SQLITE_DB'"
  exit 1
fi

DB_NAME='zoomhub_import'

dropdb --if-exists "$DB_NAME"
createdb "$DB_NAME"
stack exec migrate-database "$DB_NAME" migrate

CRITERIA='WHERE state = "completed:success" ORDER BY content.id'
# CRITERIA=''
LIMIT='LIMIT 10000'
# LIMIT=''

SET_COUNTERS_SQL=$(cat <<-EOM
-- NOTE: Adding explicit IDs does not update the serial counters for each table:
-- https://stackoverflow.com/a/244265/125305

SELECT setval('content_id_seq', COALESCE((SELECT MAX(id) + 1 FROM content), 1), false);
SELECT setval('image_content_id_seq', COALESCE((SELECT MAX(content_id) + 1 FROM image), 1), false);
SELECT setval('flickr_content_id_seq', COALESCE((SELECT MAX(content_id) + 1 FROM flickr), 1), false);
EOM
)

CONTENT_COLUMNS='id,hashId,typeId,url,state,content.initializedAt,activeAt,completedAt,title,attributionText,attributionLink,mime,size,error,progress,abuseLevelId,numAbuseReports,numViews,version'
IMAGE_COLUMNS='image.contentId,image.initializedAt,image.width,image.height,image.tileSize,image.tileOverlap,image.tileFormat'

INSERT_SQL=$(
  sqlite3 "$SQLITE_DB" '.mode insert content' "SELECT $CONTENT_COLUMNS FROM content LEFT JOIN image ON content.id = image.contentId $CRITERIA,(image.width * image.height) DESC $LIMIT" |
  (cat && sqlite3 "$SQLITE_DB" '.mode insert image' "SELECT $IMAGE_COLUMNS FROM image LEFT JOIN content ON image.contentId = content.id $CRITERIA $LIMIT") |
  (cat && sqlite3 "$SQLITE_DB" '.mode insert flickr' "SELECT contentId,farmId,serverId,photoId,secret,sizeId,(isPublic || '') AS isPublic,licenseId,originalExtension,originalSecret,ownerNSID,ownerRealName,ownerUsername,photoPageURL FROM flickr LEFT JOIN content ON flickr.contentId = content.id $CRITERIA $LIMIT") |
  (echo 'SET session_replication_role = replica;' && cat) |
  (echo 'SET client_min_messages TO WARNING;' && cat) |
  sed "s/INSERT INTO content(id,hashId,typeId,url,state,initializedAt,activeAt,completedAt,title,attributionText,attributionLink,mime,size,error,progress,abuseLevelId,numAbuseReports,numViews,version)/INSERT INTO content(id,hash_id,type_id,url,state,initialized_at,active_at,completed_at,title,attribution_text,attribution_link,mime,size,error,progress,abuse_level_id,num_abuse_reports,num_views,version)/g;" |
  sed "s/INSERT INTO image(contentId,initializedAt,width,height,tileSize,tileOverlap,tileFormat)/INSERT INTO image(content_id,created_at,width,height,tile_size,tile_overlap,tile_format)/g;" |
  sed 's/INSERT INTO flickr(contentId,farmId,serverId,photoId,secret,sizeId,isPublic,licenseId,originalExtension,originalSecret,ownerNSID,ownerRealName,ownerUsername,photoPageURL)/INSERT INTO flickr(content_id,farm_id,server_id,photo_id,secret,size_id,is_public,license_id,original_extension,original_secret,owner_nsid,owner_real_name,owner_username,photo_page_url)/g;' |
  (cat && echo "$SET_COUNTERS_SQL")
)

# echo "$INSERT_SQL"

psql --quiet --single-transaction "$DB_NAME" <<< "$INSERT_SQL"

# psql "$DB_NAME" -c 'select * from content;'
# psql "$DB_NAME"
