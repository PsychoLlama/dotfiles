{
  nixpkgs,
  darwin,
  home-manager,
  tree-sitter-remix,
  self,
  ...
}@inputs:

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

  # TODO: Manage these in the flake as part of nixpkgs import.
  inject-overlays = {
    nixpkgs.overlays = [
      self.overlays.latest-packages
      self.overlays.vim-plugins
      tree-sitter-remix.overlays.custom-grammars
    ];
  };

  # An opinionated module enabling Nix flakes.
  nix-flakes =
    {
      config,
      pkgs,
      inputs,
      ...
    }:
    {
      nix = rec {
        package = pkgs.nixUnstable;
        settings.experimental-features = "nix-command flakes";
        nixPath = [ "nixpkgs=${registry.nixpkgs.flake}" ];

        registry = {
          nixpkgs.flake = inputs.nixpkgs-unstable;
          dotfiles.flake = self;
        };
      };
    };

  # Set reasonable defaults for home-manager as a submodule.
  hm-submodule =
    { lib, ... }:
    {
      home-manager = {
        useGlobalPkgs = lib.mkDefault true;
        useUserPackages = lib.mkDefault true;

        # Add custom dotfiles modules to the HM framework.
        sharedModules = [ self.nixosModules.home-manager ];
      };
    };
in
{
  nixosSystem =
    system: configPath:
    nixpkgs.lib.nixosSystem {
      inherit system;

      specialArgs = makeSpecialArgs system;

      modules = [
        inputs.home-manager.nixosModules.home-manager
        self.nixosModules.nixos

        nix-flakes
        hm-submodule
        inject-overlays
        (makeHostnameConfig configPath)

        # Host config.
        configPath
      ];
    };

  darwinSystem =
    system: configPath:
    darwin.lib.darwinSystem {
      inherit system;

      specialArgs = makeSpecialArgs system;

      modules = [
        inputs.home-manager.darwinModules.home-manager
        self.nixosModules.darwin

        nix-flakes
        hm-submodule
        inject-overlays
        (makeHostnameConfig configPath)

        # Host config.
        configPath
      ];
    };

  homeManagerConfiguration =
    system: configPath:
    home-manager.lib.homeManagerConfiguration {
      pkgs = inputs.nixpkgs.legacyPackages.${system};
      extraSpecialArgs = makeSpecialArgs system;

      modules = [
        self.nixosModules.home-manager
        inject-overlays

        (
          { pkgs, ... }:
          {
            nix = {
              package = pkgs.nixUnstable;
              settings.experimental-features = "nix-command flakes";
            };
          }
        )

        # Host config.
        configPath
      ];
    };
}
