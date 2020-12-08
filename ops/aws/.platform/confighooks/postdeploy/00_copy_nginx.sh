#!/usr/bin/env bash

PREVIOUS_CONFIG_PATH="$HOME/nginx.conf"
CONFIG_PATH='/etc/nginx/nginx.conf'

# Overwrite default configuration

if [ -f "$PREVIOUS_CONFIG_PATH" ] && [ -f "$CONFIG_PATH" ]; then
    cat "$PREVIOUS_CONFIG_PATH" > "$CONFIG_PATH"
fi

service nginx reload