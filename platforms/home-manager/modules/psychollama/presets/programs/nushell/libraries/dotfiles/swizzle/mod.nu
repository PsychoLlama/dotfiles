# Temporarily replace nix-managed config files with editable local copies.

use ./core.nu *

# Swap a nix-managed file for a writable local copy.
export def 'main' [
  path: string@'nu-complete-swizzle-targets'
] {
  let absolute = ($path | path expand --no-symlink)
  let state = (gather-state $absolute)
  let plan = (plan-swizzle $state $absolute)

  if 'error' in $plan {
    error make { msg: $plan.error }
  }

  apply-actions $plan.actions
}

# Restore a swizzled file to its nix-managed symlink.
export def 'swizzle revert' [
  path: string@'nu-complete-swizzle-targets'
] {
  let absolute = ($path | path expand --no-symlink)
  let state = (gather-state $absolute)
  let plan = (plan-unswizzle $state $absolute)

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

  {
    exists: $main.exists
    is_symlink: $main.is_symlink
    link_target: $main.link_target
    bak_exists: $bak.exists
    bak_is_symlink: $bak.is_symlink
    bak_link_target: $bak.link_target
    copy_exists: ((do --ignore-errors { $sidecars.copy | path type }) != null)
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

def 'nu-complete-swizzle-targets' []: nothing -> list<string> {
  manifest-paths
}
