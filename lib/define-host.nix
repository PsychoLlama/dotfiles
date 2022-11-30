inputs: system: path:

# Injects dotfiles, flake inputs, and baseline NixOS configuration.

let
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

in mkSystem {
  inherit system;

  # Add stable and unstable package channels.
  specialArgs = { inherit system inputs; };

  modules = [
    ({ config, lib, pkgs, ... }: {
      imports = platform-specific-dotfiles;

      # Hostnames are set by the directory's name.
      networking.hostName = lib.mkDefault (baseNameOf path);

      # Set reasonable defaults for home-manager.
      home-manager = {
        useGlobalPkgs = lib.mkDefault true;
        useUserPackages = lib.mkDefault true;

        # Add custom dotfiles modules to the HM framework.
        users.${config.dotfiles.user.name}.imports =
          [ inputs.self.nixosModules.home-manager ];
      };

      # This can be removed once nix flakes ship standard.
      nix = rec {
        package = pkgs.nixUnstable;
        settings.experimental-features = "nix-command flakes";
        registry.nixpkgs.flake = inputs.${config.dotfiles.packageSet};
        nixPath = [ "nixpkgs=${registry.nixpkgs.flake}" ];
      };
    })

    # Do machine-specific configuration.
    path
  ];
}
