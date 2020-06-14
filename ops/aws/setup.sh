#!/bin/bash
set -euo pipefail

CIUSER=ci
ACCOUNTID=$ZH_AWS_ACCOUNT_ID

# Create ECR repo
aws ecr create-repository --repository-name $ZH_AWS_ECR_REPO --region us-east-2

# Create the CI user with appropricate permissions
aws iam create-user --user-name $CIUSER
aws iam attach-user-policy --user-name $CIUSER --policy-arn arn:aws:iam::aws:policy/AWSElasticBeanstalkFullAccess
aws iam attach-user-policy --user-name $CIUSER --policy-arn arn:aws:iam::aws:policy/AmazonEC2ContainerRegistryFullAccess
aws iam create-policy --policy-name deploy-bucket-policy --policy-document file://bucket-policy.json
aws iam attach-user-policy --user-name $CIUSER --policy-arn arn:aws:iam::$ZH_AWS_ACCOUNT_ID :policy/deploy-bucket-policy
aws iam create-access-key --user-name $CIUSER > ci-access-key.json

aws iam attach-role-policy --role-name aws-elasticbeanstalk-ec2-role --policy-arn arn:aws:iam::aws:policy/AmazonEC2ContainerRegistryReadOnly
