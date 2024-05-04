{
  nixpkgs,
  nixpkgs-unstable,
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

  # Support `<channel>` syntax for the repl, but pin it to the flake.
  nix-path = {
    # NOTE: `nixPath` is not supported by `home-manager`.
    nix.nixPath = [ "nixpkgs=${nixpkgs-unstable}" ];
  };

  # An opinionated module enabling Nix flakes.
  nix-flakes =
    { pkgs, ... }:
    {
      nix = {
        package = pkgs.nixUnstable;
        settings.experimental-features = "nix-command flakes";
        registry = {
          nixpkgs.flake = nixpkgs-unstable;
          dotfiles.flake = self;
        };
      };
    };

  # Set reasonable defaults for home-manager as a submodule.
  hm-substrate =
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
    {
      system,
      host,
      modules,
    }:
    nixpkgs.lib.nixosSystem {
      inherit system;

      modules = modules ++ [
        home-manager.nixosModules.home-manager
        self.nixosModules.nixos

        nix-path
        nix-flakes
        hm-substrate
        inject-overlays
        (makeHostnameConfig host)

        # Host config.
        host
      ];
    };

  darwinSystem =
    system: configPath:
    darwin.lib.darwinSystem {
      inherit system;

      specialArgs = makeSpecialArgs system;

      modules = [
        home-manager.darwinModules.home-manager
        self.nixosModules.darwin

        nix-path
        nix-flakes
        hm-substrate
        inject-overlays
        (makeHostnameConfig configPath)

        # Host config.
        configPath
      ];
    };

  homeManagerConfiguration =
    system: configPath:
    home-manager.lib.homeManagerConfiguration {
      pkgs = nixpkgs.legacyPackages.${system};
      extraSpecialArgs = makeSpecialArgs system;

      modules = [
        self.nixosModules.home-manager
        inject-overlays
        nix-flakes

        # Host config.
        configPath
      ];
    };
}
