#!/bin/sh


PGHOST='localhost' \
PGPORT='9001' \
  ./migrate-database "$PGDATABASE" status
