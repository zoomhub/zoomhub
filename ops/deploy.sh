#!/bin/sh

remote="$1"
if [ -z "$remote" ] ; then
  echo "Please provide host for deployment: $0 <host>"
  exit 1
fi

scp $CIRCLE_ARTIFACTS/zoomhub.keter circleci_deploy@$remote:/opt/keter/incoming
