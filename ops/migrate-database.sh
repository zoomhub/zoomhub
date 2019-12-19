#!/bin/bash
set -euo pipefail

echo "PGHOST: $PGHOST"
# echo "PGPORT: $PGPORT"
# echo "PGUSER: $PGUSER"
echo "PGDATABASE: $PGDATABASE"

PGHOST='localhost' \
PGPORT='9001' \
PGUSER='zoomhub_admin' \
  ./migrate-database "$PGDATABASE" status
