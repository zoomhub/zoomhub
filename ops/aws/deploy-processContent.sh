#!/bin/bash
set -euo pipefail

aws lambda update-function-code \
		--region us-east-2 \
		--zip-file fileb://function-processContent.zip \
		--function-name processContent
