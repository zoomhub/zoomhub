#!/bin/bash
set -eo pipefail


if [[ -f ./zoomhub-api.pid ]] ; then
  set +e
  kill -9 "$(cat zoomhub-api.pid)" >/dev/null 2>&1
  set -e
fi

# Use `jq` with both JSON and non-JSON lines.
function lenient_jq {
    jq \
      --color-output \
      --raw-output \
      --raw-input \
      "${1:-.} as \$line | try fromjson catch \$line"
}


dropdb --if-exists "$DEVELOPMENT_DB_NAME"
createdb "$DEVELOPMENT_DB_NAME"

PGUSER="$(whoami)" HASHIDS_SALT='secret-salt' \
  stack build \
    --fast \
    --no-run-tests \
    --ghc-options='-Wall -fllvm' \
    --exec "migrate-database $DEVELOPMENT_DB_NAME migrate"

# NOTE: Preserves existing content IDs:
DISABLE_PG_TRIGGERS='SET session_replication_role = replica;'
psql --output /dev/null --quiet "$DEVELOPMENT_DB_NAME" \
  <<< $(echo "$DISABLE_PG_TRIGGERS" && cat ./data/zoomhub_data.sql)
psql --output /dev/null --quiet "$DEVELOPMENT_DB_NAME" \
  < ./data/zoomhub_sequences.sql

# jq: Ignore non-JSON lines using `fromjson?`: https://blog.nem.ec/code-snippets/jq-ignore-errors/
ZH_ENV='development' \
BASE_URI="${BASE_URI:-http://localhost:8000}" \
CONTENT_BASE_URI='https://cache-development.zoomhub.net/content' \
PGDATABASE='zoomhub_development' \
PGUSER="$(whoami)" \
PROCESS_CONTENT='ProcessExistingAndNewContent' \
PROCESSING_WORKERS='2' \
S3_SOURCES_BUCKET='zoomhub-sources-development' \
UPLOADS='true' \
  ghcid \
	    --command "stack ghci --ghc-options '-fllvm' zoomhub --main-is zoomhub:exe:zoomhub" \
	    --test ZoomHub.Web.MainDevelopment.update \
	    --warnings \
	    --restart ./zoomhub.cabal \
	    --restart ./stack.yaml \
  | lenient_jq "$@" &

echo $! > zoomhub-api.pid
