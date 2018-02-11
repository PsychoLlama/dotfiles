#!/usr/bin/env bash
./ci/setup-s3.sh
aws s3 sync s3://llama-dotfiles/ ./

# Create a new release target.
git config --local user.name "Travis CI"
git config --local user.email "Jesse_Gibson@me.com"
git tag "$(date +"%Y.%m.%d__%I.%M%p")"
