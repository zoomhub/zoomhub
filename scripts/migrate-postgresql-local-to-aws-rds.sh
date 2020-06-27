#!/bin/bash
set -eo pipefail

pg_restore \
  --verbose \
  --disable-triggers \
  --data-only \
  --single-transaction \
  --table=content \
  --table=image \
  --table=flickr \
  --dbname="$PGDATABASE" \
  $1
