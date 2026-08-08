{ config, inputs, ... }:

# The base substrate every machine is built on: nixpkgs configuration, the Nix
# daemon, the Home Manager bridge, and the `programs.editor` option.
#
# Ported from `lib/hosts.nix`, which wrapped `lib.nixosSystem` rather than
# contributing modules. Kept as one file for now: several of these settings do
# not merge across definitions -- `nixpkgs.config` merges with `//`, so a
# second `allowUnfreePredicate` would silently clobber this one -- so splitting
# the concerns apart needs a design pass of its own.

let
  inherit (inputs)
    agenix
    home-manager
    nixpkgs-unstable
    self
    ;

  # Bound here because the modules below shadow `config` with their own.
  flake-modules = config.flake.modules;
  flake-overlays = config.flake.overlays;

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
          class = "editor";

          specialArgs = {
            inherit pkgs;
          };

          modules = [
            flake-modules.editor.default
            flake-modules.generic.default

            {
              # Inherit trusted directories from the home-manager platform; the
              # editor's own namespace derives `env.trusted` from them.
              psychollama.trusted-directories = lib.mkDefault config.psychollama.trusted-directories;
            }
          ];
        };
      };

      config.home.packages = lib.mkIf cfg.enable [ cfg.neovim ];
    };
in

{
  flake.modules.nixos.default =
    {
      config,
      lib,
      pkgs,
      ...
    }:

    {
      imports = [
        agenix.nixosModules.default
        home-manager.nixosModules.home-manager
      ];

      nixpkgs = {
        overlays = [
          flake-overlays.unstable-packages
          flake-overlays.custom-packages
          flake-overlays.custom-patches
          flake-overlays.vim-plugins
        ];

        config = {
          # Packages with unfree licenses. To be replaced with libre alternatives.
          allowUnfreePredicate =
            pkg:
            lib.elem (lib.getName pkg) [
              "claude-code-bin"
              "spotify"
            ];
        };

        # Pin `<nixpkgs>` and `flake:nixpkgs` to match system packages.
        flake = {
          source = lib.mkForce nixpkgs-unstable; # Stable is dumb. Live a little.
          setNixPath = lib.mkForce true;
          setFlakeRegistry = true;
        };
      };

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

      # Set reasonable defaults for home-manager as a submodule.
      home-manager = {
        useGlobalPkgs = lib.mkDefault true;
        useUserPackages = lib.mkDefault true;

        # Add custom dotfiles modules to the HM framework.
        sharedModules = [
          agenix.homeManagerModules.default
          flake-modules.homeManager.default
          flake-modules.generic.default
          editor-program

          {
            # Inherit theme config from host platform.
            theme = {
              name = lib.mkDefault config.theme.name;
              palettes = lib.mkDefault config.theme.palettes;
            };

            # Inherit identity from host platform.
            psychollama.identity = lib.mapAttrs (_: lib.mkDefault) config.psychollama.identity;

            # Inherit trusted directories from host platform.
            psychollama.trusted-directories = lib.mkDefault config.psychollama.trusted-directories;
          }
        ];
      };
    };
}
