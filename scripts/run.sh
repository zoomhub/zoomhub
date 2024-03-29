#!/bin/bash
set -eo pipefail

if [[ -z "$AWS_SECRET_ACCESS_KEY" ]]; then
  echo "Please set 'AWS_SECRET_ACCESS_KEY' environment variable"
  exit 1
fi

if [[ -z "$AWS_ACCESS_KEY_ID" ]]; then
  echo "Please set 'AWS_ACCESS_KEY_ID' environment variable"
  exit 1
fi

if [[ -z "$AWS_REGION" ]]; then
  echo "Please set 'AWS_REGION' environment variable"
  exit 1
fi

if [[ -z "$API_USERNAME" ]]; then
  echo "Please set 'API_USERNAME' environment variable"
  exit 1
fi

if [[ -z "$API_PASSWORD" ]]; then
  echo "Please set 'API_PASSWORD' environment variable"
  exit 1
fi

# Set Node.js version
source ~/.nvm/nvm.sh
nvm use

function cleanup() {
  echo "$0: Terminating child script: ngrok..."
  set +e
  kill -SIGTERM "$ngrok_pid"
  wait "$ngrok_pid"
  set -e

  echo "$0: Terminating child script: concurrently..."
  set +e
  kill -SIGTERM "$concurrently_pid"
  wait "$concurrently_pid"
  set -e

  echo "$0: Child scripts terminated. Exiting."
}

trap cleanup SIGINT SIGTERM


ngrok http 8000 --log=stdout > ngrok.log &
ngrok_pid=$!

echo -n "Get ngrok public URL."
NGROK_PUBLIC_URL=""
while [ -z "$NGROK_PUBLIC_URL" ]; do
  export NGROK_PUBLIC_URL=$(
    curl \
      --silent \
      --max-time 10 \
      --connect-timeout 5 \
      http://127.0.0.1:4040/api/tunnels \
      | jq --raw-output '.tunnels[] | select(.proto == "https") | .public_url'
  )
  sleep 1
  echo -n "."
done
echo ''

BASE_URI=$NGROK_PUBLIC_URL \
PUBLIC_PATH='frontend/build' \
  npx concurrently \
    --kill-others \
    --names "api,web" \
    "./scripts/run-api-watch.sh" \
    "export API_BASE_URI=$NGROK_PUBLIC_URL && ./scripts/run-web.sh" &
concurrently_pid=$!

wait $concurrently_pid
echo "$0: Child script has stopped. Parent script will now exit."
