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

log_level() {
    if [ -n "$LOG_PATH" ] && [ -n "$DATE" ]; then
        echo "$DATE | $1: $2" | tee -a "$LOG_PATH"
    fi
}

log_debug() { log_level 'DEBUG' "$1" }
log_info() { log_level 'INFO' "$1" }
log_error() { log_level 'ERROR' "$1" }

# Variable check
log_debug "Check certbot variables"
if [ ! -n "$CERTBOT_NAME" ] || [ ! -n "$CERTBOT_EMAIL" ] || [ ! -n "$CERTBOT_DOMAINS" ]; then
    log_error 'Certbot and/or proxy server information is missing.'
    exit 1
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
        log_error 'Changing server name limit failed'
        exit 1
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
        log_error 'nginx configuration is invalid.'
        exit 1
    fi
else
    log_error 'Certbot installation may have failed.'
    exit 1
fi

# cron: Attempt to renew certificates twice a day (to account for revocations, etc.)
cat >> /etc/cron.d/certbot_renew << END_CRON
MAILTO="$CERTBOT_EMAIL"
42 2,14 * * * root certbot renew --quiet --no-self-upgrade --deploy-hook "service nginx reload && service nginx restart"
END_CRON
chmod +x /etc/cron.d/certbot_renew

log_info 'Script ran successfully.'
