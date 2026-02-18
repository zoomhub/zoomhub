#!/bin/bash
set -eo pipefail

DEVELOPMENT_DB_NAME='zoomhub_development'

dropdb --if-exists "$DEVELOPMENT_DB_NAME"
createdb "$DEVELOPMENT_DB_NAME"

PGUSER="$(whoami)" HASHIDS_SALT='secret-salt' \
  stack build \
    --fast \
    --no-run-tests \
    --ghc-options='-Wall' \
    --exec "migrate-database $DEVELOPMENT_DB_NAME migrate"

# NOTE: Preserves existing content IDs:
DISABLE_PG_TRIGGERS='SET session_replication_role = replica;'
psql --output /dev/null --quiet "$DEVELOPMENT_DB_NAME" \
  <<< $(echo "$DISABLE_PG_TRIGGERS" && cat ./data/zoomhub_data.sql)
psql --output /dev/null --quiet "$DEVELOPMENT_DB_NAME" \
  < ./data/zoomhub_sequences.sql
