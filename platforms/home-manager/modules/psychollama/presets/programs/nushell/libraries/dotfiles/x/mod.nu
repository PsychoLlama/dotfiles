# Drop into a shell providing nix packages.
export def "main" [
  ...packages: string # Anything from nixpkgs.
] {
  # Assume we're in a dev environment.
  if ($packages | is-empty) {
    nix develop --command $env.SHELL
    return
  }

  # Default bare identifiers to the nixpkgs flake.
  def canonicalize [ident: string] {
    if ($ident | str contains "#") {
      $ident
    } else {
      $"unstable#($ident)"
    }
  }

  nix shell ...(
    | [ ...$packages ]
    | each {|ident| canonicalize $ident }
  )
}

# Show information about a nix package.
export def "x show" [
  # Any attribute of `pkgs`
  pkg_path: string

  # Show all the metadata
  --long (-l)

  # Open the homepage
  --open (-o)
] {
  let pkg = nix eval --offline --json $"nixpkgs#($pkg_path).meta" | from json

  # Probably because the package doesn't exist. Nix would've printed an error.
  if $pkg == null {
    return
  }

  if $long {
    return $pkg
  }

  if $open {
    start $pkg.homepage
    return $pkg.homepage
  }

  $pkg
  | select name? description? homepage
  | transpose key value
  | where value != null
  | reduce --fold {} {|row, acc| $acc | merge { $row.key: $row.value } }
}

# Drop into a Nix repl
export def "x repl" [] {
  nix repl -f 'flake:unstable' --extra-experimental-features 'pipe-operators'
}

# Search for packages
export def "x find" [term: string, ...extra_terms: string] {
  let result = nix search unstable $term ...$extra_terms --json | from json

  $result
  | transpose pkg meta
  | upsert pkg { split row . | skip 2 | str join . }
  | upsert meta { select description version }
}

export alias "x s" = x show
export alias "x r" = x repl
export alias "x f" = x find
