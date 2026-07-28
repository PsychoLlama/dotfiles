# A `SessionStart` hook that gives agent shells the project's direnv
# environment. `$CLAUDE_ENV_FILE` is sourced by the Bash tool, so appending an
# `eval` there re-exports the `.envrc` per command instead of freezing a
# snapshot at session start. Outside a session that sets the variable, no-op.

{
  writers,
  direnv,
  lib,
}:

let
  direnvExe = lib.getExe direnv;
in

writers.writeDash "inject-direnv" ''
  [ -z "''${CLAUDE_ENV_FILE:-}" ] && exit 0

  cat >> "''$CLAUDE_ENV_FILE" <<DIRENV
  eval "\$(${direnvExe} export bash 2>/dev/null)"
  DIRENV
''
