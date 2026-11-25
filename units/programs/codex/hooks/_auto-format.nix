# Codex `PostToolUse` auto-format hook: after an `apply_patch`, run treefmt over
# the files it touched -- the codex-side mirror of Claude Code's auto-format
# hook.
#
# Two differences from Claude shape the script:
#
#   1. The file-editing tool is `apply_patch` (Claude splits edits into
#      `Write`/`Edit`, which codex exposes only as matcher aliases). A single
#      call can touch many files, so we format a *set*, not one `file_path`.
#   2. There is no `tool_input.file_path`. The changed paths only appear in
#      `tool_response`, a JSON string holding apply_patch's summary:
#
#          Success. Updated the following files:
#          A added/file
#          M modified/file
#          D deleted/file
#
#      So we format the `A`/`M` paths (a deleted file can't be formatted) and
#      skip the rest. Relative paths resolve against the session cwd, which
#      codex sets as the hook's working directory.
#
# treefmt comes from the project devshell, not this preset; when it's absent we
# do nothing rather than failing the hook.
#
# The `--stdin` shebang binds the payload to `$in` inside `main`. That matters
# for more than convenience: consuming `$in` at a script's top level and then
# spawning an external command (treefmt) trips a nushell IR-compilation bug, so
# the `main` wrapper keeps `$in` out of top-level scope.

{
  lib,
  writeScriptBin,
  nushell,
}:

let
  nu = lib.getExe nushell;
in

writeScriptBin "codex-auto-format-hook"
  # nu
  ''
    #!/usr/bin/env -S ${nu} --stdin --no-config-file

    def main [] {
      let payload = ($in | from json)

      if (which treefmt | is-not-empty) {
        let files = (
          $payload.tool_response?
          | default ""
          | lines
          | each { str trim }
          | where {|line| ($line | str starts-with "A ") or ($line | str starts-with "M ") }
          | each {|line| $line | str substring 2.. | str trim | path expand }
          | where {|file| $file | path exists }
        )

        # Swallow treefmt's output and any failure: PostToolUse treats non-empty
        # stdout as (potential) hook JSON, and a formatter error must not block.
        if ($files | is-not-empty) {
          try { treefmt ...$files | ignore }
        }
      }
    }
  ''
