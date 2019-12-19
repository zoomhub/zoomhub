#!/bin/sh

set -eo pipefail

PGHOST='localhost' \
PGPORT='9001' \
  ./migrate-database "$PGDATABASE" status
