#!/bin/bash -ex

# Creates Keter bundle of app.
# See: https://github.com/snoyberg/keter#bundles

zoomhub=$(find .stack-work/dist -type f -name zoomhub | tr -d '\n')

rm -rf .keter-bundle
mkdir -p .keter-bundle
cp "$zoomhub" .keter-bundle
cp -R public .keter-bundle
cp -R config .keter-bundle
cd .keter-bundle
tar czfv zoomhub.keter zoomhub public config
cd -
mv .keter-bundle/zoomhub.keter .
rm -rf .keter-bundle
