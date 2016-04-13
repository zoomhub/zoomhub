#!/bin/sh

echo 'UPDATE content SET state="initialized", activeAt=NULL, completedAt=NULL, error=NULL, mime=NULL, size=NULL, progress=0.0, numViews=0;' | sqlite3 data/zoomhub-development.sqlite3
