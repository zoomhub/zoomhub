#!/bin/bash -ex

# Creates Keter bundle of app.
# See: https://github.com/snoyberg/keter#bundles


# Find app binary:
zoomhub=$(find .stack-work/dist -type f -name zoomhub | tr -d '\n')

# Remove existing bundle:
rm -rf .keter-bundle

# Create container:
mkdir -p .keter-bundle

# Copy contents:
cp "$zoomhub" .keter-bundle
cp -R public .keter-bundle
cp -R config .keter-bundle
cp -R scripts .keter-bundle

# Create bundle:
cd .keter-bundle
tar czfv zoomhub.keter zoomhub public config scripts
cd -

# Move bundle to root:
mv .keter-bundle/zoomhub.keter .

# Remove bundle
rm -rf .keter-bundle
