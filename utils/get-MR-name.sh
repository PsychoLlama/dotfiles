#!/usr/bin/env bash
set -e

function get_mr_url {
  local branch="$(git rev-parse --abbrev-ref HEAD)"
  local remote="$(git remote get-url origin)"
  local url
  url="$("$(dotfiles dir)/utils/process-mr-remote.js" "$remote" "$branch")"

  if [[ "$?" != 0 ]]; then
    echo "Url couldn't be resolved."
    echo "Check the origin remote url for anything wonky."
    return 1
  fi

  echo "$url"
}

get_mr_url
