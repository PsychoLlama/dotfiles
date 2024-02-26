{ nixpkgs, darwin, home-manager, ... }@inputs:

# Wraps the system builders for NixOS, Darwin, and Home Manager to inject the
# dotfiles framework and provide base configuration.

let
  makeSpecialArgs = system: { inherit system inputs; };

  # Assign the hostname based on the config's directory name.
  #
  # Example:
  #  ./hosts/host_abc123/default.nix -> host_abc123
  makeHostnameConfig = configPath: {
    networking.hostName = nixpkgs.lib.mkDefault (baseNameOf configPath);
  };

  # An opinionated module enabling Nix flakes.
  nix-flakes = { config, pkgs, inputs, ... }: {
    nix = rec {
      package = pkgs.nixUnstable;
      settings.experimental-features = "nix-command flakes";
      nixPath = [ "nixpkgs=${registry.nixpkgs.flake}" ];

      registry = {
        nixpkgs.flake = inputs.${config.dotfiles.packageSet};
        dotfiles.flake = inputs.self;
      };
    };
  };

  # Set reasonable defaults for home-manager as a submodule.
  hm-submodule = { config, lib, pkgs, ... }: {
    home-manager = {
      useGlobalPkgs = lib.mkDefault true;
      useUserPackages = lib.mkDefault true;

      # Add custom dotfiles modules to the HM framework.
      users.${config.dotfiles.user.name}.imports =
        [ inputs.self.nixosModules.home-manager ];
    };
  };

in {
  nixosSystem = system: configPath:
    nixpkgs.lib.nixosSystem {
      inherit system;

      specialArgs = makeSpecialArgs system;

      modules = [
        inputs.home-manager.nixosModules.home-manager
        inputs.self.nixosModules.nixos

        nix-flakes
        hm-submodule
        (makeHostnameConfig configPath)

        # Host config.
        configPath
      ];
    };

  darwinSystem = system: configPath:
    darwin.lib.darwinSystem {
      inherit system;

      specialArgs = makeSpecialArgs system;

      modules = [
        inputs.home-manager.darwinModules.home-manager
        inputs.self.nixosModules.darwin

        nix-flakes
        hm-submodule
        (makeHostnameConfig configPath)

        # Host config.
        configPath
      ];
    };

  homeManagerConfiguration = system: configPath:
    home-manager.lib.homeManagerConfiguration {
      pkgs = inputs.nixpkgs.legacyPackages.${system};
      extraSpecialArgs = makeSpecialArgs system;

      modules = [
        inputs.self.nixosModules.home-manager

        ({ pkgs, ... }: {
          # TODO: Allow configuring the package set through home-manager.
          # Currently it assumes a NixOS/Darwin layer.
          nixpkgs.overlays = [
            inputs.self.overlays.latest-packages
            inputs.self.overlays.vim-plugins
          ];

          nix = {
            package = pkgs.nixUnstable;
            settings.experimental-features = "nix-command flakes";
          };
        })

        # Host config.
        configPath
      ];
    };
}
