# Upgrade a number of packages to their bleeding edge versions.
#
# Signature: inputs.nixpkgs-unstable => overlay
nixpkgs-unstable: self: pkgs: rec {
  unstable = nixpkgs-unstable.legacyPackages.${pkgs.system};

  # Some modules cannot be overridden at the module level.
  inherit (unstable) bat exa;

  # Be reckless, run all vim plugins as latest.
  vimPlugins = unstable.vimPlugins;
}
