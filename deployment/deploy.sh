#!/bin/sh

scp $CIRCLE_ARTIFACTS/zoomhub root@104.130.230.137:/var/apps/zoomhub
scp -r $CIRCLE_ARTIFACTS/public root@104.130.230.137:/var/apps/zoomhub
