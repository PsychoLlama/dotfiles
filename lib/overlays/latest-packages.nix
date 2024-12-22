# Upgrade a number of packages to their bleeding edge versions.
#
# Signature: inputs.nixpkgs-unstable => overlay
flake-inputs: final: prev: {
  # Provides `pkgs.unstable`.
  unstable = import flake-inputs.nixpkgs-unstable {
    inherit (prev) system config;
    overlays = [
      flake-inputs.self.overlays.vim-plugins
    ];
  };
}
