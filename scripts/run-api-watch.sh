#!/bin/bash
set -eo pipefail

DEVELOPMENT_DB_NAME='zoomhub_development'

# Use `jq` with both JSON and non-JSON lines.
function lenient_jq {
    jq \
      --color-output \
      --raw-output \
      --raw-input \
      "${1:-.} as \$line | try fromjson catch \$line"
}

PGUSER="$(whoami)" HASHIDS_SALT='secret-salt' \
  stack build \
    --fast \
    --no-run-tests \
    --ghc-options='-Wall' \
    --exec "migrate-database $DEVELOPMENT_DB_NAME migrate"

# jq: Ignore non-JSON lines using `fromjson?`: https://blog.nem.ec/code-snippets/jq-ignore-errors/
BASE_URI="${BASE_URI:-http://localhost:8000}"

ZH_ENV='development' \
CONTENT_BASE_URI='https://cache-development.zoomhub.net/content' \
PGDATABASE='zoomhub_development' \
PGUSER="$(whoami)" \
PROCESS_CONTENT='ProcessExistingAndNewContent' \
PROCESSING_WORKERS='2' \
S3_SOURCES_BUCKET='zoomhub-sources-development' \
KINDE_REDIRECT_URI="$BASE_URI/auth/callbacks/" \
KINDE_LOGOUT_REDIRECT_URI="$BASE_URI" \
UPLOADS='true' \
  ghcid \
	    --command "stack ghci zoomhub --main-is zoomhub:exe:zoomhub" \
	    --test ZoomHub.Web.MainDevelopment.update \
	    --warnings \
	    --restart ./zoomhub.cabal \
	    --restart ./stack.yaml \
  | lenient_jq "$@" &
ghcid_pid=$!

function cleanup() {
  echo "$0: Terminating child script..."

  set +e
  kill -SIGTERM "$ghcid_pid"
  wait "$ghcid_pid"
  set -e

  echo "$0: Child script terminated. Exiting."
}

trap cleanup SIGINT SIGTERM

wait $ghcid_pid
echo "$0: Child script has stopped. Parent script will now exit."
