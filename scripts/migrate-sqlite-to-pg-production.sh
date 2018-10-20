#!/bin/sh

pgloader \
  --type sqlite \
  ./data/zoomhub-production-2017-01-18T05-42-32Z.sqlite3 \
  postgresql://$(whoami):@localhost/zoomhub_production

psql --dbname=zoomhub_test --file=./scripts/schema-7-pg.sql
