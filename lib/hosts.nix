{
  nixpkgs,
  nixpkgs-unstable,
  darwin,
  home-manager,
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
  hm-substrate = {
    home-manager = {
      useGlobalPkgs = lib.mkDefault true;
      useUserPackages = lib.mkDefault true;

      # Add custom dotfiles modules to the HM framework.
      sharedModules = [ self.nixosModules.home-manager ];
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
        self.nixosModules.nixos

        nix-path
        nix-flakes
        hm-substrate

        (manage-system-name hostName)
      ];
    }
  );

  darwin = lib.mapAttrs (
    hostName:
    { pkgs, modules }:
    darwin.lib.darwinSystem {
      inherit pkgs;

      modules = modules ++ [
        home-manager.darwinModules.home-manager
        self.nixosModules.darwin

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
        self.nixosModules.home-manager
        nix-flakes
      ];
    }
  );
}
