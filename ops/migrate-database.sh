#!/bin/sh

set -eo pipefail

echo "PGDATABASE: $PGDATABASE"
echo "PGUSER: $PGUSER"

PGHOST='localhost' \
PGPORT='9001' \
  ./migrate-database "$PGDATABASE" status
