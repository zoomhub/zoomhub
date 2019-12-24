#!/bin/bash
set -eo pipefail

SQLITE_DB="$1"

if [ -z "$SQLITE_DB" ]; then
  echo "Missing 'SQLITE_DB'"
  exit 1
fi

DRY_RUN="$2"

DB_NAME='zoomhub_import'

dropdb --if-exists "$DB_NAME"
createdb "$DB_NAME"
stack exec migrate-database "$DB_NAME" migrate

LIMIT='LIMIT 1'
# LIMIT=''

SET_COUNTERS_SQL=$(cat <<-EOM
-- NOTE: Adding explicit IDs does not update the serial counters for each table:
-- https://stackoverflow.com/a/244265/125305

SELECT setval('content_id_seq', COALESCE((SELECT MAX(id) + 1 FROM content), 1), false);
SELECT setval('image_content_id_seq', COALESCE((SELECT MAX(content_id) + 1 FROM image), 1), false);
SELECT setval('flickr_content_id_seq', COALESCE((SELECT MAX(content_id) + 1 FROM flickr), 1), false);
EOM
)

CONTENT_COLUMNS_SQLITE='id,hashId,typeId,url,state,initializedAt,activeAt,completedAt,title,attributionText,attributionLink,mime,size,error,progress,abuseLevelId,numAbuseReports,numViews,version'
CONTENT_COLUMNS_PG='id,hash_id,type_id,url,state,initialized_at,active_at,completed_at,title,attribution_text,attribution_link,mime,size,error,progress,abuse_level_id,num_abuse_reports,num_views,version'

IMAGE_COLUMNS_SQLITE='contentId,initializedAt,width,height,tileSize,tileOverlap,tileFormat'
IMAGE_COLUMNS_PG='content_id,created_at,width,height,tile_size,tile_overlap,tile_format'

FLICKR_COLUMNS_SQLITE='contentId,farmId,serverId,photoId,secret,sizeId,isPublic,licenseId,originalExtension,originalSecret,ownerNSID,ownerRealName,ownerUsername,photoPageURL'
FLICKR_COLUMNS_PG='content_id,farm_id,server_id,photo_id,secret,size_id,is_public,license_id,original_extension,original_secret,owner_nsid,owner_real_name,owner_username,photo_page_url'

sqlite3 "$SQLITE_DB" '.mode insert content' "SELECT $CONTENT_COLUMNS_SQLITE FROM content $LIMIT" | \
  (cat && sqlite3 "$SQLITE_DB" '.mode insert image' "SELECT $IMAGE_COLUMNS_SQLITE FROM image $LIMIT") | \
  (cat && sqlite3 "$SQLITE_DB" '.mode insert flickr' "SELECT $(echo $FLICKR_COLUMNS_SQLITE | sed "s/isPublic/(isPublic || '') AS isPublic/") FROM flickr $LIMIT") | \
  (echo 'SET session_replication_role = replica;' && cat) | \
  (echo 'SET client_min_messages TO WARNING;' && cat) | \
  sed "s/INSERT INTO content($CONTENT_COLUMNS_SQLITE)/INSERT INTO content($CONTENT_COLUMNS_PG)/g;" | \
  sed "s/INSERT INTO image($IMAGE_COLUMNS_SQLITE)/INSERT INTO image($IMAGE_COLUMNS_PG)/g;" | \
  sed "s/INSERT INTO flickr($FLICKR_COLUMNS_SQLITE)/INSERT INTO flickr($FLICKR_COLUMNS_PG)/g;" | \
  (cat && echo "$SET_COUNTERS_SQL") | \
  psql --quiet --single-transaction "$DB_NAME"
