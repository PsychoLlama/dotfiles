# Pure planning + classification for swizzle.
# These commands are total functions over plain records. No I/O.

# Derive sidecar paths from a target path.
export def 'sidecar-paths' [path: string]: nothing -> record {
  {
    backup: $"($path).bak"
    copy: $"($path).swizzled"
  }
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
# `state` carries the same fields as `classify` plus `copy_exists`. Returns
# `{ actions: [...] }` on success or `{ error: <message> }` on refusal.
export def 'plan-swizzle' [state: record, path: string]: nothing -> record {
  let kind = (classify $state)

  if $kind != 'nix-managed' {
    return { error: $"Cannot swizzle: ($path) is ($kind), expected nix-managed." }
  }

  let sidecars = (sidecar-paths $path)

  if ($state.copy_exists? | default false) {
    return { error: $"Cannot swizzle: ($sidecars.copy) already exists." }
  }

  {
    actions: [
      { op: 'copy-content', from: $path,             to: $sidecars.copy }
      { op: 'move',         from: $path,             to: $sidecars.backup }
      { op: 'symlink',      target: ($sidecars.copy | path basename), link_path: $path }
    ]
  }
}

# Plan the action list to revert a swizzled target.
export def 'plan-unswizzle' [state: record, path: string]: nothing -> record {
  let kind = (classify $state)

  if $kind != 'swizzled' {
    return { error: $"Cannot revert: ($path) is ($kind), expected swizzled." }
  }

  let sidecars = (sidecar-paths $path)

  {
    actions: [
      { op: 'remove', path: $sidecars.copy }
      { op: 'remove', path: $path }
      { op: 'move',   from: $sidecars.backup, to: $path }
    ]
  }
}
