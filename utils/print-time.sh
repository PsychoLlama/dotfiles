#!/usr/bin/env bash

# Print a fancy date, like "18:38 Tuesday"
function tmux_get_time_status {
  local time="$(date +%R)"
  local day="$(date +%A)"

  echo "$time $day"
}

tmux_get_time_status
