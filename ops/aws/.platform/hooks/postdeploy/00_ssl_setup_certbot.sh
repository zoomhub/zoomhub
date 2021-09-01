#!/bin/bash

# MIT License
#
# Copyright (c) 2020 Anthony Le <https://github.com/HausCloud/AWS-EB-SSL>
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

# IMPORTANT: use this bash otherwise following error: "failed with error exit status 127. Stderr:/usr/bin/env: bash: No such file or directory"
# IMPORTANT: use LF instead of CRLF for .sh files, otherwise following error: "00_ssl_setup_certbot.sh: no such file or directory"
# IMPORTANT: for LF: also set "* text eol=lf" in ".gitattributes" file otherwise git will convert it into CRLF again on Windows :(

set -euo pipefail

# Postdeploy script for enabling SSL (single instance)
# Compatible only with Amazon Linux 2 EC2 instances

LOG_PATH=$(find /var/log/ -type f -iname 'eb-hooks.log')
DATE=$(date -u +"%Y-%m-%dT%H:%M:%SZ")

# IMPORTANT: no whitespaces in CERTBOT_NAME, otherwise following error: "invalid number of arguments in "ssl_certificate" directive in /etc/nginx/nginx.conf:81"
CERTBOT_NAME="$CERTBOT_NAME"
CERTBOT_EMAIL="$CERTBOT_EMAIL"
# Multiple domain example: CERTBOT_DOMAINS='bort.com,www.bort.com,bort-env.eba-2kg3gsq2.us-east-2.elasticbeanstalk.com'
CERTBOT_DOMAINS="$CERTBOT_DOMAINS"

log_level() {
    if [ -n "$LOG_PATH" ]; then
        DATE=$(date -u +"%Y-%m-%dT%H:%M:%SZ")
        echo "$DATE | $1: $2" | tee -a "$LOG_PATH"
    fi
}

log_debug() { log_level 'DEBUG' "$1"; }
log_info() { log_level 'INFO' "$1"; }
log_error() { log_level 'ERROR' "$1"; }

# Variable check
log_debug "Check certbot variables"
if [ -z "$CERTBOT_NAME" ] || [ -z "$CERTBOT_EMAIL" ] || [ -z "$CERTBOT_DOMAINS" ]; then
    log_error 'Certbot and/or proxy server information is missing.'
    exit 1
fi

# WORKAROUND: Stderr:Repodata is over 2 weeks old. Install yum-cron? Or run: yum makecache fast
yum makecache fast

# Install EPEL
# Source: https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/amazon-linux-ami-basics.html
log_debug "yum: Install EPEL"
if ! yum list installed epel-release; then
    yum install -y https://dl.fedoraproject.org/pub/epel/epel-release-latest-7.noarch.rpm
fi

# Install certbot
log_debug "yum: Install certbot"
if yum list installed epel-release && ! command -v certbot &>/dev/null; then
    yum install -y certbot python2-certbot-nginx
fi


# HTTP_STRING='^http\s*{$'
# NAME_LIMIT='http {\nserver_names_hash_bucket_size 192;\n'

# # Prevent replace if not clean sample app
# if ! grep -Fxq "$NAME_LIMIT" /etc/nginx/nginx.conf; then
#     # Increase size of string name for --domains (for default EB configs)

#     log_debug "nginx: Increase name limit"
#     if ! sed -i "s/$HTTP_STRING/$NAME_LIMIT/g" /etc/nginx/nginx.conf; then
#         log_error 'Changing server name limit failed'
#         exit 1
#     fi
# fi

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
        log_error 'Nginx configuration is invalid.'
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
