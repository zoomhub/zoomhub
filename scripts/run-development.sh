#!/bin/sh

[ -f ./zoomhub.pid ] && kill $(cat zoomhub.pid)
HASHIDS_SALT='DEVELOPMENT-ONLY' stack exec zoomhub &
echo $! > zoomhub.pid
