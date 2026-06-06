# Temporarily replace nix-managed config files with editable local copies.
export def main [] {
  help modules swizzle
}

# Manifest of nix-managed file paths, written by `psychollama.manifest`.
const MANIFEST = ($nu.home-dir | path join '.config' 'swizzle' 'manifest.json')

# Swap a nix-managed file for a writable local copy in the current directory.
export def 'swizzle edit' [
  path: string@'nu-complete-swizzle-targets'
] {
  let absolute = ($path | path expand --no-symlink)
  let dest = ($env.PWD | path join ($absolute | path basename))

  let kind = (classify $absolute)
  if $kind != 'nix-managed' {
    error make {
      msg: $"Cannot swizzle: ($absolute) is ($kind), expected nix-managed."
      label: { text: $kind, span: (metadata $path).span }
    }
  }

  if (path-kind $dest) != null {
    error make {
      msg: $"Cannot swizzle: ($dest) already exists."
      label: { text: 'copy already exists here', span: (metadata $path).span }
    }
  }

  open --raw $absolute | save --force $dest
  mv $absolute $"($absolute).bak"
  ln --symbolic --force $dest $absolute
}

# Restore swizzled files to their nix-managed symlinks, returning the files
# reverted. Reverts every swizzled file when no path is given.
export def 'swizzle revert' [
  path?: string@'nu-complete-swizzled-targets'
]: nothing -> list<string> {
  let targets = [$path]
    | compact
    | default --empty (manifest-paths | where { |p| (classify $p) == 'swizzled' })

  mut reverted = []
  for p in $targets {
    let absolute = ($p | path expand --no-symlink)
    let kind = (classify $absolute)
    if $kind != 'swizzled' {
      error make {
        msg: $"Cannot revert: ($absolute) is ($kind), expected swizzled."
        label: { text: $kind, span: (metadata $path).span }
      }
    }

    rm (link-target $absolute)
    rm $absolute
    mv $"($absolute).bak" $absolute
    $reverted = ($reverted | append $absolute)
  }

  $reverted
}

# List every file in the manifest alongside its current state:
#
#   nix-managed — symlink into the store; ready to swizzle.
#   swizzled    — symlink to a local copy, with the store symlink backed up.
#   foreign     — not in either expected shape (e.g. a real file or stray link).
#   missing     — nothing exists at the path.
export def 'swizzle list' [] {
  manifest-paths | each { |p| { path: $p, state: (classify $p) } }
}

# Resolve a path's type, or null when it doesn't exist.
def 'path-kind' [path: string]: nothing -> any {
  do --ignore-errors { $path | path type }
}

# Resolve a symlink's target to an absolute path.
def 'link-target' [path: string]: nothing -> string {
  let target = (readlink $path | str trim)
  if ($target | str starts-with '/') {
    $target
  } else {
    ($path | path dirname | path join $target)
  }
}

# Classify the shape of a target: 'nix-managed' | 'swizzled' | 'foreign' | 'missing'.
#
# A nix-managed file is a symlink into the store with no backup. A swizzled file
# is a symlink to a local copy whose `.bak` is the original store symlink.
def 'classify' [path: string]: nothing -> string {
  if (path-kind $path) == null {
    return 'missing'
  }

  if (path-kind $path) != 'symlink' {
    return 'foreign'
  }

  let store = '/nix/store/'
  let bak = $"($path).bak"

  let points_to_store = ((link-target $path) | str starts-with $store)
  let bak_points_to_store = (
    ((path-kind $bak) == 'symlink')
      and ((link-target $bak) | str starts-with $store)
  )

  if $points_to_store and ((path-kind $bak) == null) {
    return 'nix-managed'
  }

  if $bak_points_to_store and (not $points_to_store) {
    return 'swizzled'
  }

  'foreign'
}

# Paths exported by `psychollama.manifest`, minus the manifest file itself.
def 'manifest-paths' []: nothing -> list<string> {
  if not ($MANIFEST | path exists) {
    return []
  }

  open $MANIFEST
    | get path
    | where { |p| $p != $MANIFEST }
}

# Compress an absolute path to use `~` when it lives under the home directory.
def 'tilde-path' [p: string]: nothing -> string {
  let home = $nu.home-dir
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
    | where { |p| (classify $p) == 'swizzled' }
    | each { tilde-path $in }
}
