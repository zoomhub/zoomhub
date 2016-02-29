#!/bin/sh

if [[ -f ./zoomhub.pid ]] ; then
  kill $(cat zoomhub.pid)
fi

BASE_URI='http://localhost:8000' \
HASHIDS_SALT='DEVELOPMENT-ONLY' \
  stack exec zoomhub &

echo $! > zoomhub.pid
