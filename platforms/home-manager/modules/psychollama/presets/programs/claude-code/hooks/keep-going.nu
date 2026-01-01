#!/usr/bin/env -S nu --stdin

# Keep Claude running on a task indefinitely.
# Great for long-running migrations.
@example "Tell claude to keep going" {
  CLAUDE_CONTINUE="Keep going!" claude
}
export def main [] {
  let input = $in | from json
  let prompt = $env.CLAUDE_CONTINUE?

  # Prompt not set. Skip the hook.
  if ($prompt | is-empty) {
    exit 0
  }

  let output = {
    decision: "block"
    reason: $prompt
  }

  $output | to json | print --stderr $in

  exit 2
}
