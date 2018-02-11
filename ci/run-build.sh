#!/usr/bin/env bash
./setup-s3.sh
./framework-setup.sh
cargo build --verbose --release --color always
aws s3 cp ./target/release/dotfiles \
  "s3://llama-dotfiles/dotfiles-$TRAVIS_OS_NAME"
