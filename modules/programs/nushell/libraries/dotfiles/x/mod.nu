# Drop into a shell providing nix packages.
export def "main" [
  ...packages: string # Anything from nixpkgs.
] {
  # Drop into a repl with the current flake.
  if ($packages | is-empty) {
    x repl
    return
  }

  # Default bare identifiers to the nixpkgs flake.
  def canonicalize [ident: string] {
    if ($ident | str contains '#') {
      $ident
    } else {
      $"unstable#($ident)"
    }
  }

  nix shell ...(
    | [...$packages]
    | each { |ident| canonicalize $ident }
  ) --command nu
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
    | select name? description? homepage?
    | transpose key value
    | where value != null
    | reduce --fold {} { |row, acc| $acc | merge { $row.key: $row.value } }
}

# Drop into a Nix repl
export def 'x repl' [] {
  nix repl -f 'flake:unstable' --extra-experimental-features 'pipe-operators'
}

# Search for packages
export def 'x find' [
  term: string # Search query (regex supported by search.nixos.org).
  ...extra_terms: string # Additional terms to narrow the search.
] {
  nh search --json $term ...$extra_terms
  | from json
  | get results
  | select package_attr_name package_pversion package_description
  | rename pkg version description
}

export alias 'x s' = x show
export alias 'x r' = x repl
export alias 'x f' = x find
