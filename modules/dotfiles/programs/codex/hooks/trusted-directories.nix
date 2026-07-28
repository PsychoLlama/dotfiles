# A codex `SessionStart` hook that pre-seeds directory trust so codex stops
# prompting to trust repositories under `psychollama.trusted-directories`.
#
# Codex doesn't have a way to say "this directory tree is trusted". It forces
# you to approve every single project. Absurd! I've got too many repos to
# bother with that.
#
# This is evil and gross but on startup, we enumerate every project and inject
# them into the codex config as a trusted directory.
#
# `directories` are the raw `~`-relative (or absolute) roots from
# `psychollama.trusted-directories`; `~` is expanded at runtime against $HOME so
# a single /etc config stays correct per-user.

{
  lib,
  writers,
  fd,
  formats,
  directories,
}:

let
  # Emit the roots as a JSON file up front and open it as data at runtime,
  # rather than assuming JSON happens to be valid nushell source.
  rootsFile = (formats.json { }).generate "codex-trusted-roots.json" directories;
in

writers.writeNuBin "codex-trusted-directories-hook"
  # nu
  ''
    let codex_home = ($env.CODEX_HOME? | default ($env.HOME | path join ".codex"))
    let config_file = ($codex_home | path join "config.toml")

    # Trusted roots from `psychollama.trusted-directories`. Each holds a single
    # layer of repositories ($root/$repo); `~` is expanded at runtime.
    let roots = (open ${rootsFile} | each { path expand })

    # Every repository one level under a trusted root. --no-ignore so a stray
    # .ignore/.gitignore can't hide a repo. A missing root is a harmless fd
    # warning; the hook is only wired when there's at least one root, so fd
    # always has a search path and never falls back to scanning the cwd.
    let trusted = (
      ^${lib.getExe fd} --type dir --max-depth 1 --no-ignore --absolute-path . ...$roots
      | lines
      | each { str trim --right --char "/" }
      | sort
    )

    # Read codex's config as real data, merge trust in, and write it back only if
    # something actually changed. codex owns this file, so we preserve everything
    # it wrote and just add the projects it hasn't trusted yet.
    let existing = (if ($config_file | path exists) { open $config_file } else { {} })

    mut projects = ($existing.projects? | default {})
    for dir in $trusted {
      $projects = ($projects | upsert $dir { trust_level: "trusted" })
    }

    let updated = ($existing | upsert projects $projects)
    if $updated != $existing {
      mkdir $codex_home
      $updated | save --force $config_file
    }
  ''
