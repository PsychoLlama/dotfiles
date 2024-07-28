# Upgrade a number of packages to their bleeding edge versions.
#
# Signature: inputs.nixpkgs-unstable => overlay
nixpkgs-unstable: final: prev: rec {
  unstable = import nixpkgs-unstable { inherit (prev) system config; };

  # Be reckless, run all vim plugins as latest.
  vimPlugins = unstable.vimPlugins;
}
