#!/bin/sh

if [[ -f ./zoomhub.pid ]] ; then
  kill $(cat zoomhub.pid)
fi


# # Find app binary:
# zoomhub=$(find .stack-work/dist -type f -name zoomhub | tr -d '\n')

# # Self-sign app to avoid constant Mac OS X firewall warnings:
# codesign --force --sign zoomhub.net "$zoomhub"

BASE_URI='http://localhost:8000' \
HASHIDS_SALT='DEVELOPMENT-ONLY' \
  stack exec zoomhub &

echo $! > zoomhub.pid
