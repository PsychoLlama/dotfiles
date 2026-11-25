# A `history`-like interface for all commands executed by agents.
# Only populated when `AGENT_HISTORY_PATH` is set and hooks are enabled.

export-env {
  $env.AGENT_HISTORY_PATH = $env.AGENT_HISTORY_PATH? | default (
    $nu.home-dir | path join agents history.sqlite3
  )
}

# Show module help.
export def main [] {
  help modules agent
}

# Commands run by agents, oldest first. Mirrors the `history` builtin.
export def 'agent history' [
  --long (-l) # Show every column.
]: nothing -> table {
  let db = $env.AGENT_HISTORY_PATH
  if not ($db | path exists) {
    error make {
      msg: $"No agent history at ($db)."
      help: "Create it with `agent history init`."
    }
  }

  let rows = open $db | query db "select * from history order by id"
    | each {|row|
      {
        agent: $row.agent
        item_id: $row.id
        start_timestamp: ($row.start_timestamp * 1_000_000 | into datetime)
        command: $row.command_line
        session_id: $row.session_id
        cwd: $row.cwd
        duration: ($row.duration_ms * 1ms)
        exit_status: $row.exit_status
        tool_use_id: $row.tool_use_id
      }
    }

  if $long {
    $rows
  } else {
    $rows | select start_timestamp command cwd duration exit_status
  }
}

# Create an empty agent history database.
export def 'agent history init' [
  path?: path # Defaults to $env.AGENT_HISTORY_PATH.
  --force (-f) # Replace an existing database.
] {
  let db = $path | default $env.AGENT_HISTORY_PATH | path expand
  if ($db | path exists) {
    if not $force {
      error make {
        msg: $"($db) already exists."
        help: "Pass --force to replace it."
      }
    }

    rm --force $db $"($db)-wal" $"($db)-shm"
  }

  mkdir ($db | path dirname)

  # `query db` needs an existing database; seed one and drop the seed table.
  { seed: 0 } | into sqlite $db --table-name seed
  [
    "drop table seed"
    "
      create table history (
        agent text not null,
        id integer primary key autoincrement,
        command_line text not null,
        start_timestamp integer,
        session_id text,
        cwd text,
        duration_ms integer,
        exit_status integer,
        tool_use_id text
      ) strict
    "
    "create index idx_history_agent on history(agent)"
    "create index idx_history_time on history(start_timestamp)"
    "create index idx_history_cwd on history(cwd)"
    "create index idx_history_exit_status on history(exit_status)"
    "create index idx_history_cmd on history(command_line)"
    "create index idx_history_session on history(session_id)"
  ] | each {|sql| open $db | query db $sql } | ignore
}
