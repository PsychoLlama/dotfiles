{
  home-manager,
  nix-darwin,
  nixpkgs,
  nixpkgs-unstable,
  self,
  ...
}:

# Wraps the system builders for NixOS, Darwin, and Home Manager to inject the
# dotfiles framework and provide base configuration.

let
  inherit (nixpkgs) lib;

  manage-system-name = hostName: {
    # The hostname determines the default attrset key to use when rebuilding
    # the system.
    networking.hostName = lib.mkDefault hostName;
  };

  # Pin `<nixpkgs>` and `flake:nixpkgs` to match system packages. This is the
  # default on NixOS 24.05.
  nix-path = {
    nix = {
      nixPath = [ "nixpkgs=${nixpkgs-unstable}" ];
      registry.nixpkgs.flake = nixpkgs-unstable;
    };
  };

  # An opinionated module enabling Nix flakes.
  nix-flakes =
    { pkgs, ... }:
    {
      nix = {
        package = pkgs.nixVersions.latest;
        settings.experimental-features = "nix-command flakes";
        registry.dotfiles.flake = self;
      };
    };

  # Provides `programs.editor` (Neovim config).
  editor-program =
    {
      lib,
      config,
      pkgs,
      ...
    }:

    let
      cfg = config.programs.editor;
    in

    {
      options.programs.editor = lib.mkOption {
        description = "Configure and install Neovim";
        default = { };
        type = lib.types.submoduleWith {
          specialArgs = {
            inherit pkgs;
          };

          modules = [
            self.nixosModules.editor-platform
            self.nixosModules.editor-configs
          ];
        };
      };

      config.home.packages = lib.mkIf cfg.enable [ cfg.neovim ];
    };

  # Set reasonable defaults for home-manager as a submodule.
  hm-substrate = {
    home-manager = {
      useGlobalPkgs = lib.mkDefault true;
      useUserPackages = lib.mkDefault true;

      # Add custom dotfiles modules to the HM framework.
      sharedModules = [
        self.nixosModules.home-manager-platform
        self.nixosModules.home-manager-configs
        editor-program
      ];
    };
  };

in

{
  nixos = lib.mapAttrs (
    hostName:
    { pkgs, modules }:
    lib.nixosSystem {
      inherit pkgs;

      modules = modules ++ [
        home-manager.nixosModules.home-manager
        self.nixosModules.nixos-platform
        self.nixosModules.nixos-configs

        nix-flakes
        hm-substrate

        (manage-system-name hostName)
      ];
    }
  );

  darwin = lib.mapAttrs (
    hostName:
    { pkgs, modules }:
    nix-darwin.lib.darwinSystem {
      inherit pkgs;

      modules = modules ++ [
        home-manager.darwinModules.home-manager

        nix-path
        nix-flakes
        hm-substrate

        (manage-system-name hostName)
      ];
    }
  );

  home-manager = lib.mapAttrs (
    hostName:
    { pkgs, modules }:
    home-manager.lib.homeManagerConfiguration {
      inherit pkgs;

      modules = modules ++ [
        self.nixosModules.home-manager-platform
        self.nixosModules.home-manager-configs
        nix-flakes
      ];
    }
  );
}
