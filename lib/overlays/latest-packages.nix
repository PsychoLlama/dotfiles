# Upgrade a number of packages to their bleeding edge versions.
#
# Signature: inputs.nixpkgs-unstable => overlay
nixpkgs-unstable: self: pkgs: rec {
  unstable = nixpkgs-unstable.legacyPackages.${pkgs.system};

  # Be reckless, run all vim plugins as latest.
  vimPlugins = unstable.vimPlugins;
}
