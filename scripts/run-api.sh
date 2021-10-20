#!/bin/bash
set -eo pipefail


if [[ -f ./zoomhub-api.pid ]] ; then
  set +e
  kill -9 $(cat zoomhub-api.pid) >/dev/null 2>&1
  set -e
fi

ZH_ENV='development' \
BASE_URI="${BASE_URI:-http://localhost:8000}" \
CONTENT_BASE_URI='https://cache-development.zoomhub.net/content' \
PGDATABASE='zoomhub_development' \
PGUSER="$(whoami)" \
PROCESS_CONTENT='ProcessExistingAndNewContent' \
PROCESSING_WORKERS='2' \
S3_SOURCES_BUCKET='zoomhub-sources-development' \
UPLOADS='true' \
  stack exec zoomhub | jq &

echo $! > zoomhub-api.pid
