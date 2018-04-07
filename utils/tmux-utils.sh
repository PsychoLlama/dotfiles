#!/usr/bin/env bash

# Print a fancy date, like "6:38pm Tuesday"
function tmux_get_time_status {
  local hours="$(date +%I | sed 's/^0//')"
  local minutes="$(date +%M)"
  local day="$(date +%A)"

  # Mac's `date` doesn't support %P (lowercased am/pm).
  local ampm="$(date +%p | tr "[:upper:]" "[:lower:]")"

  echo "$hours:$minutes$ampm $day"
}
