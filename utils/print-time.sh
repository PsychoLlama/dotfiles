#!/usr/bin/env bash

# Print a fancy date, like "18:38 Tuesday"
function tmux_get_time_status {
  local time="$(date +%l:%M)"
  local day="$(date +%A)"

  # Cross-platform lowercase am/pm requires some slight hackery.
  local am_pm="$(date +%p | tr '[:upper:]' '[:lower:]')"

  echo "${time/ /}${am_pm} ${day}"
}

tmux_get_time_status
