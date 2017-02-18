#!/bin/sh

pgloader \
  ./data/zoomhub-development.sqlite3 \
  postgresql://$(whoami):@localhost/zoomhub-development
