#!/bin/sh

zoomhub=$(find .stack-work/dist -type f -name zoomhub | tr -d '\n')
cp -R public $CIRCLE_ARTIFACTS
cp "$zoomhub" $CIRCLE_ARTIFACTS