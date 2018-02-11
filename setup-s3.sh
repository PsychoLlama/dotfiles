#!/usr/bin/env bash
set -e

# Check if it's already installed.
if which aws &> /dev/null; then
  exit 0
fi

sudo easy_install awscli

# Add credentials
if [[ ! -f ~/.aws/credentials ]]; then
  mkdir -p ~/.aws
  echo "[default]" > ~/.aws/credentials
  echo "aws_access_key_id = $AWS_ACCESS_ID" >> ~/.aws/credentials
  echo "aws_secret_access_key = $AWS_ACCESS_KEY" >> ~/.aws/credentials
fi
