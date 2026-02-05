{
  agenix,
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

  nixpkgs-config.nixpkgs = {
    overlays = [
      self.overlays.latest-packages
      self.overlays.packages
      self.overlays.vim-plugins
    ];

    config = {
      # Packages with unfree licenses. To be replaced with libre alternatives.
      allowUnfreePredicate =
        pkg:
        lib.elem (lib.getName pkg) [
          "claude-code" # symlinkJoin wrapper from home-manager
          "claude-code-bin"
        ];
    };

    # Pin `<nixpkgs>` and `flake:nixpkgs` to match system packages.
    flake = {
      source = lib.mkForce nixpkgs-unstable; # Stable is dumb. Live a little.
      setNixPath = lib.mkForce true;
      setFlakeRegistry = true;
    };
  };

  # An opinionated module enabling Nix flakes.
  nix-flakes =
    { pkgs, ... }:
    {
      nix = {
        package = pkgs.nixVersions.latest;

        registry = {
          dotfiles.flake = self;
          unstable.flake = nixpkgs-unstable;
        };

        settings = {
          experimental-features = "nix-command flakes";
          flake-registry = null; # Disable default listings.
        };
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
  hm-substrate =
    { config, ... }:
    {
      home-manager = {
        useGlobalPkgs = lib.mkDefault true;
        useUserPackages = lib.mkDefault true;

        # Add custom dotfiles modules to the HM framework.
        sharedModules = [
          agenix.homeManagerModules.default
          self.nixosModules.home-manager-platform
          self.nixosModules.home-manager-configs
          self.nixosModules.theme
          editor-program

          {
            # Inherit theme config from host platform.
            theme = {
              name = lib.mkDefault config.theme.name;
              palettes = lib.mkDefault config.theme.palettes;
            };
          }
        ];
      };
    };

in

{
  nixos = lib.mapAttrs (
    hostName: modules:
    lib.nixosSystem {
      modules = modules ++ [
        agenix.nixosModules.default
        home-manager.nixosModules.home-manager
        self.nixosModules.nixos-platform
        self.nixosModules.nixos-configs
        self.nixosModules.theme

        nixpkgs-config
        nix-flakes
        hm-substrate

        (manage-system-name hostName)
      ];
    }
  );

  darwin = lib.mapAttrs (
    hostName: modules:
    nix-darwin.lib.darwinSystem {
      modules = modules ++ [
        home-manager.darwinModules.home-manager
        self.nixosModules.theme

        nixpkgs-config
        nix-flakes
        hm-substrate

        (manage-system-name hostName)
      ];
    }
  );

  # NOTE: Requires an import of `pkgs` ahead of time. No support for
  # dynamically importing with `nixpkgs.hostPlatform`.
  home-manager = lib.mapAttrs (
    hostName:
    { system, modules }:
    home-manager.lib.homeManagerConfiguration {
      pkgs = import nixpkgs nixpkgs-config.nixpkgs // {
        inherit system;
      };

      modules = modules ++ [
        agenix.homeManagerModules.default
        self.nixosModules.home-manager-platform
        self.nixosModules.home-manager-configs
        self.nixosModules.theme
        nix-flakes
        editor-program
      ];
    }
  );
}
