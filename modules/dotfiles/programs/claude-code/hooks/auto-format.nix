# A `PostToolUse` hook that runs treefmt over the file Claude just edited.
#
# treefmt comes from the project devshell, not this preset; when it's absent we
# do nothing rather than failing the hook. Formatter output and failures are
# swallowed for the same reason.

{
  lib,
  writers,
  jq,
}:

let
  jqExe = lib.getExe jq;
in

writers.writeDash "auto-format"
  # sh
  ''
    command -v treefmt >/dev/null 2>&1 || exit 0

    file_path=$(${jqExe} -r '.tool_input.file_path // ""')

    [ -z "$file_path" ] || [ ! -f "$file_path" ] && exit 0

    treefmt "$file_path" 2>/dev/null || true
  ''
