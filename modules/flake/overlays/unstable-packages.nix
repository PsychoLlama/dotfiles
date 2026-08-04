{ config, inputs, ... }:

{
  # Upgrade a number of packages to their bleeding edge versions.
  flake.overlays.unstable-packages = final: prev: {
    # Provides `pkgs.unstable`.
    unstable = import inputs.nixpkgs-unstable {
      inherit (prev.stdenv.hostPlatform) system;
      inherit (prev) config;

      # Every overlay but this one, which would recurse forever.
      overlays = [
        config.flake.overlays.custom-packages
        config.flake.overlays.custom-patches
        config.flake.overlays.vim-plugins
      ];
    };
  };
}
