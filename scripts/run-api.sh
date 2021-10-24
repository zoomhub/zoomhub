#!/bin/bash
set -eo pipefail


if [[ -f ./zoomhub-api.pid ]] ; then
  set +e
  kill -9 $(cat zoomhub-api.pid) >/dev/null 2>&1
  set -e
fi

PGDATABASE='zoomhub_development' \
PGUSER="$(whoami)" \
ZH_BASE_URI="${ZH_BASE_URI:-http://localhost:8000}" \
ZH_CONTENT_BASE_URI='https://cache-development.zoomhub.net/content' \
ZH_ENV='development' \
ZH_PROCESS_CONTENT='ProcessExistingAndNewContent' \
ZH_PROCESSING_WORKERS='2' \
ZH_S3_SOURCES_BUCKET='zoomhub-sources-development' \
ZH_UPLOADS='true' \
  stack exec zoomhub | jq &

echo $! > zoomhub-api.pid
