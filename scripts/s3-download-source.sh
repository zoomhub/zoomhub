#!/bin/bash
set -euo pipefail

aws s3 cp --profile zoomhub s3://zoomhub-sources/uploads/$1 .
