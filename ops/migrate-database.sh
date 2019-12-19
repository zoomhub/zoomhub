#!/bin/sh

if [ -z "$PGHOST" ] ; then
  echo "Please provide database host for migration: PGHOST"
  exit 1
fi

PGHOST='localhost' \
PGPORT='9001' \
HASHIDS_SALT='secret-salt' \
  ./migrate-database "$PGDATABASE" status
