#!/bin/bash
set -euo pipefail

# Install from: https://github.com/cannikin/great-migration
cd ../great-migration

bin/greatmigration \
  --rackspace-user=zoomingservice \
  --rackspace-key="$RACKSPACE_API_KEY" \
  --rackspace-container="$RACKSPACE_CONTAINER" \
  --rackspace-region=iad \
  --aws-key="$AWS_ACCESS_KEY_ID" \
  --aws-secret="$AWS_SECRET_ACCESS_KEY" \
  --aws-bucket="$AWS_S3_BUCKET" \
  --aws-region=us-east-2

cd -
