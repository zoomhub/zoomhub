#!/bin/bash
set -euo pipefail

SHA1=$1

# Push image to ECR (AWS CLI version 2)
aws ecr get-login-password \
    --region us-east-2 \
| docker login \
    --username AWS \
    --password-stdin $ZH_AWS_ACCOUNT_ID.dkr.ecr.us-east-2.amazonaws.com

docker push $ZH_AWS_ACCOUNT_ID.dkr.ecr.us-east-2.amazonaws.com/$ZH_AWS_ECR_REPO:$SHA1

# Create new Elastic Beanstalk version
EB_BUCKET=elasticbeanstalk-$ZH_AWS_EB_PROJECT-deploy-bucket
SOURCE_BUNDLE_ZIP="$SHA1"_source_bundle.zip

sed "s/<TAG>/$SHA1/ ; s/<ZH_AWS_ACCOUNT_ID>/$ZH_AWS_ACCOUNT_ID/ ; s/<ZH_AWS_ECR_REPO>/$ZH_AWS_ECR_REPO/" < Dockerrun.aws.json.template > Dockerrun.aws.json

# NOTE: For `.ebextensions` and `.platform` to be included, we must not include
# them inside the Docker image, but rather in the source ZIP file bundle:
# - https://stackoverflow.com/a/30926732/125305
# - https://gist.github.com/ianblenke/dec31660e170cfdfc7d3
# - https://serverfault.com/questions/887912/using-ebextensions-with-docker-in-aws-elasticbeanstalk
zip -r $SOURCE_BUNDLE_ZIP Dockerrun.aws.json .ebextensions .platform

aws s3 cp \
  $SOURCE_BUNDLE_ZIP \
  s3://$EB_BUCKET/$SOURCE_BUNDLE_ZIP \
  --region us-east-2
aws elasticbeanstalk create-application-version \
    --application-name $ZH_AWS_EB_PROJECT \
    --version-label $SHA1 \
    --source-bundle S3Bucket=$EB_BUCKET,S3Key=$SOURCE_BUNDLE_ZIP \
    --region us-east-2

# Update Elastic Beanstalk environment to new version
aws elasticbeanstalk update-environment \
    --environment-name $ZH_AWS_EB_ENVIRONMENT \
    --version-label $SHA1 \
    --region us-east-2
