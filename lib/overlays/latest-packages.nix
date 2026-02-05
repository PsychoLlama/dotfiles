# Upgrade a number of packages to their bleeding edge versions.
#
# Signature: inputs.nixpkgs-unstable => overlay
flake-inputs: final: prev: {
  # Provides `pkgs.unstable`.
  unstable = import flake-inputs.nixpkgs-unstable {
    inherit (prev) config;
    system = prev.stdenv.hostPlatform.system;
    overlays = [
      flake-inputs.self.overlays.packages
      flake-inputs.self.overlays.vim-plugins
    ];
  };
}
