#!/bin/bash
set -euo pipefail

# Postdeploy script for enabling SSL (single instance)
# Compatible only with Amazon Linux 2 EC2 instances

LOG_PATH=$(find /var/log/ -type f -iname 'eb-hooks.log')
DATE=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

CERTBOT_NAME="$CERTBOT_NAME"
CERTBOT_EMAIL="$CERTBOT_EMAIL"
# Multiple domain example: CERTBOT_DOMAINS='bort.com,www.bort.com,bort-env.eba-2kg3gsq2.us-east-2.elasticbeanstalk.com'
CERTBOT_DOMAINS="$CERTBOT_DOMAINS"

crontab_exists() {
    crontab -u root -l 2>/dev/null | grep 'certbot renew --quiet' >/dev/null 2>/dev/null
}

log_level() {
    if [ -n "$LOG_PATH" ] && [ -n "$DATE" ]; then
        echo "$DATE | $1: $2" | tee -a "$LOG_PATH"
    fi
}

log_debug() {
    log_level "DEBUG" "$1"
}

log_and_exit() {
    log_level "$1" "$2"
    exit 0
}

# Variable check

log_debug "Check certbot variables"
if [ ! -n "$CERTBOT_NAME" ] || [ ! -n "$CERTBOT_EMAIL" ] || [ ! -n "$CERTBOT_DOMAINS" ]; then
    log_and_exit 'INFO' 'Certbot and/or proxy server information is missing.'
fi

# Auto allow yes for all yum install
# SUGGESTION: Remove after deployment
log_debug "yum: assumeyes=1"
if ! grep -q 'assumeyes=1' /etc/yum.conf; then
    echo 'assumeyes=1' | tee -a /etc/yum.conf
fi

# Install EPEL
# Source: https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/amazon-linux-ami-basics.html
log_debug "yum: Install EPEL"
if ! yum list installed epel-release; then
    yum install https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm
fi

# Install certbot
log_debug "yum: Install certbot"
if yum list installed epel-release && ! command -v certbot &>/dev/null; then
    yum install certbot python2-certbot-nginx
fi


HTTP_STRING='^http\s*{$'
NAME_LIMIT='http {\nserver_names_hash_bucket_size 192;\n'

# Prevent replace if not clean sample app
if ! grep -Fxq "$NAME_LIMIT" /etc/nginx/nginx.conf; then
    # Increase size of string name for --domains (for default EB configs)

    log_debug "nginx: Increase name limit"
    if ! sed -i "s/$HTTP_STRING/$NAME_LIMIT/g" /etc/nginx/nginx.conf; then
        log_and_exit 'ERROR' 'Changing server name limit failed'
    fi
fi

# Set up certificates
if command -v certbot &>/dev/null; then
    log_debug "nginx: Check configuration"
    if nginx -t; then
        log_debug "certbot: Install nginx configuration"
        certbot --nginx \
          --cert-name "$CERTBOT_NAME" \
          --email "$CERTBOT_EMAIL" \
          --domains "$CERTBOT_DOMAINS" \
          --redirect \
          --agree-tos \
          --no-eff-email \
          --keep-until-expiring \
          --non-interactive
    else
        log_and_exit 'ERROR' 'Nginx configuration is invalid.'
    fi
else
    log_and_exit 'ERROR' 'Certbot installation may have failed.'
fi

# Create cron task (attempt) to renew certificate every 29 days
log_debug "crontab: Check if exists"
if ! crontab_exists; then
    systemctl start crond
    systemctl enable crond

    LINE="42 2,14 * * * certbot renew --quiet --no-self-upgrade --deploy-hook \"service nginx reload && service nginx restart\""

    (
        crontab -u root -l
        echo "$LINE"
    ) | crontab -u root -
fi

log_and_exit 'INFO' 'Script ran successfully.'
