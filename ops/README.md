# Ops

## AWS

### Elastic Beanstalk

- Application logs: `tail -f /var/log/eb-docker/containers/eb-current-app/*.log`
- nginx configuration: `cat /etc/nginx/nginx.conf`
- nginx extensions: `cat /etc/nginx/conf.d/*.conf` (note `.conf` extension)
- cron logs: `sudo tail -f /var/log/cron`

#### Adding new environment

Add new rule to RDS security groups pointing to new environment, e.g. `staging-2`.

### Lambda + EFS

Requires VPC for the Lambda which cuts off internet access, i.e. the Lambda can
no longer call the ZoomHub API. This manifests itself as rather unhelpful timeout
without any log output.

To fix this, follow this guide:
https://aws.amazon.com/premiumsupport/knowledge-center/internet-access-lambda-function/

Other timeout causes:
https://aws.amazon.com/premiumsupport/knowledge-center/lambda-vpc-troubleshoot-timeout/

Security recommendations:
https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Scenario2.html#VPC_Scenario2_Security
