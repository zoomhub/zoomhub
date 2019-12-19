#!/bin/bash
set -euo pipefail

PGHOST='localhost' \
PGPORT='9001' \
PGUSER='zoomhub_admin' \
  ./migrate-database "$PGDATABASE" migrate
