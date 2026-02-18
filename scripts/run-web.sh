#!/bin/bash
set -eo pipefail

echo "Waiting for API: $API_BASE_URI"
until curl --insecure --head  --silent --fail --output /dev/null "$API_BASE_URI"/internal/config; do
  echo -n '.'
  sleep 1
done

cd frontend && \
VITE_API_BASE_URI="$API_BASE_URI" \
VITE_STATIC_BASE_URI="https://static.zoomhub.net" \
VITE_WEB_BASE_URI="$API_BASE_URI" \
  npx vite build --watch &

vite_pid=$!

function cleanup() {
  echo "$0: Terminating child script..."

  set +e
  kill -SIGTERM "$vite_pid"
  wait "$vite_pid"
  set -e

  echo "$0: Child script terminated. Exiting."
}
trap cleanup SIGINT SIGTERM

wait $vite_pid
echo "$0: Child script has stopped. Parent script will now exit."
