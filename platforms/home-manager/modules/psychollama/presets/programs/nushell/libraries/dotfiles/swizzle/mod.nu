# Temporarily replace nix-managed config files with editable local copies.

use ./core.nu *

# Swap a nix-managed file for a writable local copy in the current directory.
export def 'main' [
  path: string@'nu-complete-swizzle-targets'
] {
  let absolute = ($path | path expand --no-symlink)
  let dest = ($env.PWD | path join ($absolute | path basename))
  let state = (gather-state $absolute)
    | merge { dest_exists: ((do --ignore-errors { $dest | path type }) != null) }
  let plan = (plan-swizzle $state $absolute $dest)

  if 'error' in $plan {
    error make { msg: $plan.error }
  }

  apply-actions $plan.actions
}

# Restore a swizzled file to its nix-managed symlink.
export def 'swizzle revert' [
  path: string@'nu-complete-swizzled-targets'
  --force (-f) # Discard edits when the editable copy differs from the backup.
] {
  let absolute = ($path | path expand --no-symlink)
  let state = (gather-state $absolute)
  let plan = (plan-unswizzle $state $absolute $force)

  if 'error' in $plan {
    error make { msg: $plan.error }
  }

  apply-actions $plan.actions
}

# List currently swizzled files.
export def 'swizzle list' [] {
  manifest-paths
    | each { |p|
      { path: $p, state: (classify (gather-state $p)) }
    }
    | where state == 'swizzled'
}

# Report the current state of a path.
export def 'swizzle status' [
  path: string@'nu-complete-swizzle-targets'
] {
  let absolute = ($path | path expand --no-symlink)
  { path: $absolute, state: (classify (gather-state $absolute)) }
}

# Probe filesystem state for `classify` / `plan-*`.
def 'gather-state' [path: string]: nothing -> record {
  let sidecars = (sidecar-paths $path)

  let probe = { |p|
    let kind = (do --ignore-errors { $p | path type })
    let exists = ($kind != null)
    let is_symlink = ($kind == 'symlink')
    let target = if $is_symlink {
      (^readlink $p | str trim)
    } else { '' }
    { exists: $exists, is_symlink: $is_symlink, link_target: $target }
  }

  let main = (do $probe $path)
  let bak = (do $probe $sidecars.backup)

  # When swizzled, the editable copy lives at the symlink target. Resolve
  # relative targets against the link's directory so callers can treat
  # `copy_path` as absolute.
  let copy_path = if $main.is_symlink {
    if ($main.link_target | str starts-with '/') {
      $main.link_target
    } else {
      ($path | path dirname | path join $main.link_target)
    }
  } else { '' }

  let copy_exists = ($copy_path != '') and (
    (do --ignore-errors { $copy_path | path type }) != null
  )

  let files_differ = if $copy_exists and $bak.exists {
    (open --raw $copy_path) != (open --raw $sidecars.backup)
  } else { false }

  {
    exists: $main.exists
    is_symlink: $main.is_symlink
    link_target: $main.link_target
    bak_exists: $bak.exists
    bak_is_symlink: $bak.is_symlink
    bak_link_target: $bak.link_target
    copy_path: $copy_path
    files_differ: $files_differ
  }
}

# Interpret a plan from `core.nu`. Pure switch on `op`.
def 'apply-actions' [actions: list] {
  for action in $actions {
    match $action.op {
      'copy-content' => { open --raw $action.from | save --force $action.to }
      'move'         => { mv $action.from $action.to }
      'symlink'      => { ^ln --symbolic --force $action.target $action.link_path }
      'remove'       => { rm $action.path }
      _              => { error make { msg: $"Unknown action: ($action.op)" } }
    }
  }
}

# Paths exported by `psychollama.manifest`, minus the manifest file itself.
def 'manifest-paths' []: nothing -> list<string> {
  let manifest = ('~/.config/swizzle/manifest.json' | path expand)

  if not ($manifest | path exists) {
    return []
  }

  open $manifest
    | get path
    | where { |p| $p != $manifest }
}

# Compress an absolute path to use `~` when it lives under $HOME.
def 'tilde-path' [p: string]: nothing -> string {
  let home = $env.HOME
  if ($p | str starts-with $"($home)/") {
    '~' + ($p | str substring ($home | str length)..)
  } else if $p == $home {
    '~'
  } else {
    $p
  }
}

def 'nu-complete-swizzle-targets' []: nothing -> list<string> {
  manifest-paths | each { tilde-path $in }
}

def 'nu-complete-swizzled-targets' []: nothing -> list<string> {
  manifest-paths
    | where { |p| (classify (gather-state $p)) == 'swizzled' }
    | each { tilde-path $in }
}
