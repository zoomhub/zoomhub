#!/bin/bash
set -euo pipefail

CONFIG_PATH='/etc/nginx/nginx.conf'

# Save nginx configuration to root's home folder

if [ -f "$CONFIG_PATH" ]; then
    cp "$CONFIG_PATH" ~/
fi
