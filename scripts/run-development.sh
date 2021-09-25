#!/bin/bash
set -eo pipefail


if [[ -f ./zoomhub.pid ]] ; then
  set +e
  kill -9 $(cat zoomhub.pid) >/dev/null 2>&1
  set -e
fi

if [[ -z "$AWS_ACCESS_KEY_ID" ]]; then
  echo "Missing 'AWS_ACCESS_KEY_ID' environment variable"
  exit 1
fi

if [[ -z "$AWS_SECRET_ACCESS_KEY" ]]; then
  echo "Missing 'AWS_SECRET_ACCESS_KEY' environment variable"
  exit 1
fi

ZH_ENV='development' \
BASE_URI="${BASE_URI:-http://localhost:8000}" \
CONTENT_BASE_URI='http://cache-development.zoomhub.net/content' \
PGDATABASE='zoomhub_development' \
PGUSER="$(whoami)" \
PROCESS_CONTENT='ProcessExistingAndNewContent' \
PROCESSING_WORKERS='2' \
S3_SOURCES_BUCKET='zoomhub-sources-development' \
UPLOADS='true' \
  stack exec zoomhub | jq &

echo $! > zoomhub.pid
