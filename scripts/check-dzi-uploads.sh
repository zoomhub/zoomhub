#!/bin/bash

# set -eo pipefail

echo 'SELECT hashId FROM content WHERE state="completed:success" ORDER BY completedAt;' | \
  sqlite3 --noheader ~/zoomhub-backup/zoomhub-production-2019-11-11T00-41-37Z.sqlite3  | \
  xargs -I {} curl -s -o /dev/null -w '{} %{http_code}\n' 'http://cache.zoomhub.net/content/{}.dzi' | \
  grep -v 200
