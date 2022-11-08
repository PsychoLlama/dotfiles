inputs: system: path:

# Injects dotfiles, flake inputs, and baseline NixOS configuration.

let
  use = flake: flake.legacyPackages.${system};

  nixpkgs-unstable = use inputs.nixpkgs-unstable;
  pkgs = use inputs.nixpkgs;
  darwinModules = [
    inputs.home-manager.darwinModules.home-manager
    inputs.self.nixosModules.darwin
  ];

  nixosModules = [
    inputs.home-manager.nixosModules.home-manager
    inputs.self.nixosModules.nixos
  ];

  systems = {
    "x86_64-darwin" = inputs.darwin.lib.darwinSystem;
    "aarch64-darwin" = inputs.darwin.lib.darwinSystem;
  };

  platform-specific-dotfiles = {
    "aarch64-darwin" = darwinModules;
    "x86_64-darwin" = darwinModules;
  }.${system} or nixosModules;

  mkSystem = systems.${system} or inputs.nixpkgs.lib.nixosSystem;

in mkSystem rec {
  inherit system;

  # Add stable and unstable package channels.
  specialArgs = { inherit system inputs nixpkgs-unstable; };

  modules = [
    ({ lib, pkgs, ... }: {
      imports = platform-specific-dotfiles;

      # Hostnames are set by the directory's name.
      networking.hostName = lib.mkDefault (baseNameOf path);

      # This can be removed once nix flakes ship standard.
      nix = {
        package = pkgs.nixUnstable;
        extraOptions = ''
          experimental-features = nix-command flakes
        '';
      };
    })

    # Do machine-specific configuration.
    path
  ];
}
