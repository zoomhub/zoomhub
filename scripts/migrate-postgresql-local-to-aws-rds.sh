#!/bin/bash
set -eo pipefail

psql \
  --single-transaction \
  --file $1
