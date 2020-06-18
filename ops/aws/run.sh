#!/bin/bash
set -euo pipefail

function json_log_time() {
  echo "\"time\": \"$(date -u +"%Y-%m-%dT%H:%M:%SZ")\""
}

# TODO: Update when we add auto-scaling or retrieve value via AWS CLI:
# https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/command-options-general.html#command-options-general-elasticbeanstalkenvironment
eb_environment_type='SingleInstance'
# eb_environment_type='LoadBalanced'

is_leader=$(if [[ -f "/tmp/is_leader" ]]; then echo 'true'; else echo 'false'; fi)

echo "{$(json_log_time), \"message\": \"Run startup script\", \"script\": \"run.sh\", \"environmentType\": \"$eb_environment_type\", \"isLeader\": $is_leader}"
if [[  "$eb_environment_type" == "SingleInstance" || ("$eb_environment_type" == "LoadBalanced" && "$is_leader" == "true" ) ]]; then
  echo "{$(json_log_time), \"message\": \"Migrate database\", \"script\": \"run.sh\"}"
  /opt/zoomhub/migrate-database $PGDATABASE migrate
fi

echo "{$(json_log_time), \"message\": \"Run app\", \"script\": \"run.sh\"}"
/opt/zoomhub/zoomhub