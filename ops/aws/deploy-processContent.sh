#!/bin/bash
set -euo pipefail

if [[ -z "$ZH_ENV" ]]; then
  echo "Please set 'ZH_ENV' environment variable"
  exit 1
fi

MEMORY_SIZE=8192
NUM_CONCURRENT_UPLOADS=64
ROOT_PATH='/mnt/efs'
TILE_SIZE=254
TMPDIR='/mnt/efs/tmp'
VIPS_DISC_THRESHOLD='512m'

case $ZH_ENV in
  production)
    MEMORY_SIZE=4096
    ROOT_PATH='/tmp'
    S3_CACHE_BUCKET='cache.zoomhub.net'
    TMPDIR='/tmp'
    ZH_API_PASSWORD=$ZH_API_PASSWORD_PRODUCTION
    ZH_API_USERNAME=$ZH_API_USERNAME_PRODUCTION
    ;;
  staging)
    S3_CACHE_BUCKET='cache-staging.zoomhub.net'
    TILE_SIZE=510
    ZH_API_PASSWORD=$ZH_API_PASSWORD_STAGING
    ZH_API_USERNAME=$ZH_API_USERNAME_STAGING
    ;;
  development)
    S3_CACHE_BUCKET='cache-development.zoomhub.net'
    TILE_SIZE=510
    ZH_API_PASSWORD=$ZH_API_PASSWORD_DEVELOPMENT
    ZH_API_USERNAME=$ZH_API_USERNAME_DEVELOPMENT
    ;;
  *)
    echo "Unknown environment: $ZH_ENV"
    exit 1
    ;;
esac

if [[ -z "$ZH_API_USERNAME" ]]; then
  echo "Please set 'ZH_API_USERNAME' environment variable"
  exit 1
fi

if [[ -z "$ZH_API_PASSWORD" ]]; then
  echo "Please set 'ZH_API_PASSWORD' environment variable"
  exit 1
fi

# Source: https://stackoverflow.com/a/70474523/125305
aws lambda wait function-updated --function-name processContent

update_code_output=$(
  aws lambda update-function-code \
    --zip-file fileb://function-processContent.zip \
    --function-name processContent
)

aws lambda wait function-updated --function-name processContent

update_configuration_output=$(
  aws lambda update-function-configuration \
    --function-name processContent \
    --memory-size $MEMORY_SIZE \
    --description "$(date +%FT%T%z)-$ZH_ENV-memory-$MEMORY_SIZE" \
    --environment "Variables={NUM_CONCURRENT_UPLOADS=$NUM_CONCURRENT_UPLOADS,ROOT_PATH=$ROOT_PATH,S3_CACHE_BUCKET=$S3_CACHE_BUCKET,TMPDIR=$TMPDIR,VIPS_DISC_THRESHOLD=$VIPS_DISC_THRESHOLD,ZH_API_PASSWORD=$ZH_API_PASSWORD,ZH_API_USERNAME=$ZH_API_USERNAME}"
)

aws lambda wait function-updated --function-name processContent

publish_output=$(
  aws lambda publish-version \
      --function-name processContent \
      --code-sha256 $(jq --raw-output '.CodeSha256' <<< "$update_code_output")
)

aws lambda wait function-updated --function-name processContent

aws lambda update-alias \
    --function-name processContent \
    --function-version $(jq --raw-output '.Version' <<< "$publish_output") \
    --name $ZH_ENV

aws lambda wait function-updated --function-name processContent
