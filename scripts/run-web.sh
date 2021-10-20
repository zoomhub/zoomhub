#!/bin/bash
set -eo pipefail

if [[ -f zoomhub-web.pid ]] ; then
  set +e
  kill -9 $(cat zoomhub-web.pid) >/dev/null 2>&1
  set -e
fi


echo "Waiting for API: $API_BASE_URI"
until $(curl --insecure --head  --silent --fail --output /dev/null $API_BASE_URI/v1/config); do
  echo -n '.'
  sleep 1
done

cd frontend
SNOWPACK_PUBLIC_API_BASE_URI=$API_BASE_URI npx snowpack dev &
cd -

echo $! > zoomhub-web.pid
