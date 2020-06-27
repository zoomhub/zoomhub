#!/bin/bash
set -eo pipefail

DB_NAME='zoomhub_import-newline-fix'
# Docs: https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/PostgreSQL.Procedural.Importing.html#USER_PostgreSQL.S3Import
#   pg_dump -Fc -v -h [endpoint of instance] -U [master username] [database] > [database].dump

# NOTE: `--disable-triggers` doesn’t work on AWS RDS.
# Using `SET session_replication_role = replica;` instead:
# - https://forums.aws.amazon.com/thread.jspa?messageID=561509
# - https://aws.amazon.com/about-aws/whats-new/2014/11/10/amazon-rds-postgresql-read-replicas/

{ echo 'SET session_replication_role = replica;' \
  & \
  pg_dump \
    --format=plain \
    --data-only \
    --table=content \
    --table=image \
    --table=flickr \
    $DB_NAME;
} > zoomhub-production-$(date -u +"%Y-%m-%dT%H-%M").dump.sql
