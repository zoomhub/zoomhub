#!/bin/bash
set -eo pipefail

echo "Waiting for API: $API_BASE_URI"
until curl --insecure --head  --silent --fail --output /dev/null "$API_BASE_URI"/internal/config; do
  echo -n '.'
  sleep 1
done

SNOWPACK_PUBLIC_API_BASE_URI="$API_BASE_URI" \
SNOWPACK_PUBLIC_STATIC_BASE_URI="https://static.zoomhub.net" \
SNOWPACK_PUBLIC_WEB_BASE_URI="$API_BASE_URI" \
  npx snowpack build --watch &

snowpack_pid=$!

function cleanup() {
  echo "$0: Terminating child script..."

  set +e
  kill -SIGTERM "$snowpack_pid"
  wait "$snowpack_pid"
  set -e

  echo "$0: Child script terminated. Exiting."
}
trap cleanup SIGINT SIGTERM

wait $snowpack_pid
echo "$0: Child script has stopped. Parent script will now exit."
