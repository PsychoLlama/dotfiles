# Upgrade a number of packages to their bleeding edge versions.
#
# Signature: inputs.nixpkgs-unstable => overlay
nixpkgs-unstable: self: pkgs: rec {
  unstable = nixpkgs-unstable.legacyPackages.${pkgs.system};

  # `programs.bat.package` is not configurable. Force it.
  # TODO: Remove this evil when `home-manager/24.05` drops.
  inherit (unstable) bat;

  # Be reckless, run all vim plugins as latest.
  vimPlugins = unstable.vimPlugins;
}
