def main [] {
  let db = $env.AGENT_HISTORY_PATH?
  if $db == null { return }

  let event = $in | from json
  let now_ms = (date now | into int) // 1_000_000

  try {
    open $db | query db "
      insert into history (
        agent, command_line, start_timestamp, session_id,
        cwd, duration_ms, exit_status, tool_use_id
      ) values (?, ?, ?, ?, ?, ?, ?, ?)
    " --params [
      claude
      $event.tool_input.command
      ($now_ms - $event.duration_ms)
      $event.session_id
      $event.cwd
      $event.duration_ms
      (parse-exit-code $event)
      $event.tool_use_id
    ] | ignore
  } catch {|err|
    # History is opt-in via `agent history init`.
    if ($db | path exists) { error make $err }
  }
}

# Failure events only report the exit code inside the error message.
def parse-exit-code [event: record] {
  if $event.hook_event_name == "PostToolUse" { return 0 }

  $event.error?
  | default ""
  | parse --regex '^Exit code (?<code>\d+)'
  | get code.0?
  | if $in != null { into int }
}
