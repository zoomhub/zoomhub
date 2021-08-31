#!/bin/bash
set -euo pipefail

SHA1="$1"

docker build \
  --rm=false \
  -t $ZH_AWS_ACCOUNT_ID.dkr.ecr.us-east-2.amazonaws.com/$ZH_AWS_ECR_REPO:$SHA1 \
  -f Dockerfile.deploy .
