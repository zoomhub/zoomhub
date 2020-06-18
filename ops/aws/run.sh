#!/bin/bash
set -euo pipefail

if [ -f /tmp/is_leader ]; then
   echo '{"message": "Run startup script", "script":"run.sh", "isLeader": true}'
   /opt/zoomhub/migrate-database $PGDATABASE migrate
else
  echo '{"message": "Run startup script", "script":"run.sh", "isLeader": false}'
fi

/opt/zoomhub/zoomhub
