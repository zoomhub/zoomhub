#!/bin/sh

zoomhub=$(find .stack-work/dist -name zoomhub)
cp -R public $CIRCLE_ARTIFACTS
cp "$zoomhub" $CIRCLE_ARTIFACTS
