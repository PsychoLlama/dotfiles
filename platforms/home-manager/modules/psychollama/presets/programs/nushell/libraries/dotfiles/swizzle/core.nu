# Pure planning + classification for swizzle.
# These commands are total functions over plain records. No I/O.

# Derive sidecar paths from a target path.
export def 'sidecar-paths' [path: string]: nothing -> record {
  { backup: $"($path).bak" }
}

# Classify the current shape of a target.
#
# Input record shape:
#   { exists, is_symlink, link_target,
#     bak_exists, bak_is_symlink, bak_link_target }
#
# Output: 'nix-managed' | 'swizzled' | 'foreign' | 'missing'
export def 'classify' [state: record]: nothing -> string {
  if not $state.exists {
    return 'missing'
  }

  if not $state.is_symlink {
    return 'foreign'
  }

  let nix_store_prefix = '/nix/store/'
  let points_to_store = ($state.link_target | str starts-with $nix_store_prefix)

  let bak_points_to_store = (
    $state.bak_exists
      and $state.bak_is_symlink
      and ($state.bak_link_target | str starts-with $nix_store_prefix)
  )

  if $points_to_store and (not $state.bak_exists) {
    return 'nix-managed'
  }

  if $bak_points_to_store and (not $points_to_store) {
    return 'swizzled'
  }

  'foreign'
}

# Plan the action list to swizzle a target.
#
# `state` carries the same fields as `classify` plus `dest_exists`. `dest`
# is where the editable copy will live (typically `$PWD/(basename path)`).
# Returns `{ actions: [...] }` on success or `{ error: <message> }` on refusal.
export def 'plan-swizzle' [state: record, path: string, dest: string]: nothing -> record {
  let kind = (classify $state)

  if $kind != 'nix-managed' {
    return { error: $"Cannot swizzle: ($path) is ($kind), expected nix-managed." }
  }

  if ($state.dest_exists? | default false) {
    return { error: $"Cannot swizzle: ($dest) already exists." }
  }

  let sidecars = (sidecar-paths $path)

  {
    actions: [
      { op: 'copy-content', from: $path, to: $dest }
      { op: 'move',         from: $path, to: $sidecars.backup }
      { op: 'symlink',      target: $dest, link_path: $path }
    ]
  }
}

# Plan the action list to revert a swizzled target.
#
# `state.copy_path` is the absolute path of the editable copy (resolved from
# the symlink target). `state.files_differ` is true when the editable copy
# and the backup have different contents; when true, `force` must be set or
# revert is refused so the user's edits aren't silently discarded.
export def 'plan-unswizzle' [state: record, path: string, force: bool]: nothing -> record {
  let kind = (classify $state)

  if $kind != 'swizzled' {
    return { error: $"Cannot revert: ($path) is ($kind), expected swizzled." }
  }

  if (not $force) and ($state.files_differ? | default false) {
    return {
      error: $"Refusing to revert: the editable copy of ($path) has unsaved edits. Re-run with --force to discard them."
    }
  }

  let sidecars = (sidecar-paths $path)

  {
    actions: [
      { op: 'remove', path: $state.copy_path }
      { op: 'remove', path: $path }
      { op: 'move',   from: $sidecars.backup, to: $path }
    ]
  }
}
