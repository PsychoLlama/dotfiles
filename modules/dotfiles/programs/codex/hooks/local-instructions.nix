# A codex `SessionStart` hook that injects `CLAUDE.local.md` into context.
#
# Codex has `AGENTS.override.md`, but it *replaces* the directory's `AGENTS.md`
# instead of layering on top. I want the `CLAUDE.local.md` behaviour: a private,
# gitignored file whose contents are *appended* to the committed instructions.
# Reusing the same filename means one local file serves both Claude and codex.
#
# So on startup, when we're inside a git repo, we walk it like codex walks
# `AGENTS.md` (from the repo root down to the session cwd), read every
# `CLAUDE.local.md`, and hand them back as `additionalContext`. Codex merges
# that into the model's context. Outside a repo we do nothing.

{
  writers,
}:

writers.writeNuBin "codex-local-instructions-hook"
  # nu
  ''
    use std/iter

    # Codex runs the hook with its working directory set to the session cwd.
    let start = $env.PWD
    let home = $env.HOME

    # Every ancestor of the session cwd, ordered filesystem-root -> cwd.
    let parts = ($start | path split)
    let ancestors = (1..($parts | length) | each {|n| $parts | first $n | path join })

    # Project root: the nearest ancestor holding a `.git` marker, searched from
    # the cwd upward so the innermost repo wins and we stop the moment we find
    # one. We only consider directories strictly under $HOME and never treat
    # $HOME itself as a root, so outside a project repo -- or sitting in $HOME,
    # which may be its own git repo -- `root` is null and we inject nothing
    # rather than climbing into home-level files.
    let root = (
      $ancestors
      | where {|dir| $dir | str starts-with $"($home)/" }
      | reverse
      | iter find {|dir| $dir | path join ".git" | path exists }
    )

    if ($root | is-not-empty) {
      # Dirs from the project root down to the session cwd, inclusive, so nested
      # invocations still pick up parent files. Mirrors codex's `AGENTS.md` walk.
      let chain = ($ancestors | skip until {|dir| $dir == $root })

      # One `CLAUDE.local.md` block per dir along that chain, root-most first;
      # missing and empty files drop out.
      let blocks = (
        $chain | iter filter-map {|dir|
          let file = ($dir | path join "CLAUDE.local.md")
          if ($file | path exists) {
            let body = (open --raw $file | decode utf-8 | str trim)
            if ($body | is-empty) { null } else { $"# ($file)\n\n($body)" }
          } else {
            null
          }
        }
      )

      # Inject as SessionStart additionalContext. Wrapped in the JSON envelope
      # rather than printed raw so a file that happens to start with `{`/`[`
      # isn't mistaken for (malformed) hook JSON. Empty stdout when there's
      # nothing to add.
      if not ($blocks | is-empty) {
        let context = ($blocks | str join "\n\n---\n\n")
        {
          hookSpecificOutput: {
            hookEventName: "SessionStart",
            additionalContext: $context,
          }
        } | to json --raw | print
      }
    }
  ''
