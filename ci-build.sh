#!/usr/bin/env bash
./framework-setup.sh
cargo build --verbose --release --color always

# Add credentials
if [[ ! -f ~/.aws/credentials ]]; then
  mkdir -p ~/.aws
  echo "[default]" > ~/.aws/credentials
  echo "aws_access_key_id = $AWS_ACCESS_ID" >> ~/.aws/credentials
  echo "aws_secret_access_key = $AWS_ACCESS_KEY" >> ~/.aws/credentials
fi

aws s3 cp ./target/release/dotfiles \
  "s3://llama-dotfiles/dotfiles-$TRAVIS_OS_NAME"
