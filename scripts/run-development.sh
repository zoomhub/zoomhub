#!/bin/sh

if [[ -f ./zoomhub.pid ]] ; then
  kill $(cat zoomhub.pid)
fi


# # See: http://apple.stackexchange.com/questions/3271/how-to-get-rid-of-firewall-accept-incoming-connections-dialog/121010

# # Find app binary:
# zoomhub=$(find .stack-work/dist -type f -name zoomhub | tr -d '\n')

# # Self-sign app to avoid constant Mac OS X firewall warnings:
# sudo codesign --force --sign - "$zoomhub"

BASE_URI='http://localhost:8000' \
PGUSER=$(whoami) \
PGDATABASE='zoomhub_development' \
RACKSPACE_USERNAME='' \
RACKSPACE_API_KEY='' \
RACKSPACE_CONTAINER='cache' \
RACKSPACE_CONTAINER_PATH='content' \
  stack exec zoomhub | jq &

echo $! > zoomhub.pid
