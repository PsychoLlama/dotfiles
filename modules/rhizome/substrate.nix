{ config, inputs, ... }:

# The base substrate every machine is built on: nixpkgs configuration, the Nix
# daemon, the Home Manager bridge, and the `programs.editor` option.
#
# Lives here rather than under `aspects/` because it reads flake inputs
# (`agenix`, `home-manager`, `self`). Aspects are re-evaluated inside a
# consumer's flake, where those inputs do not exist.
#
# One file: `nixpkgs.config` merges with `//`, so a second `allowUnfreePredicate`
# would silently clobber this one. Splitting the concerns apart needs a design
# pass of its own.

let
  inherit (inputs)
    agenix
    home-manager
    nixpkgs-unstable
    self
    ;

  # Bound here because the modules below shadow `config` with their own.
  flake-outputs = config.flake;
  flake-overlays = config.flake.overlays;

  # Provides `programs.editor` (Neovim config).
  editor-program =
    {
      lib,
      config,
      pkgs,
      host,
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
            flake-outputs.editorModules.platform

            { _module.args.host = host; }
          ];
        };
      };

      config.home.packages = lib.mkIf cfg.enable [ cfg.neovim ];
    };
in

{
  flake.nixosModules.default =
    {
      lib,
      pkgs,
      host,
      ...
    }:

    {
      imports = [
        agenix.nixosModules.default
        home-manager.nixosModules.home-manager
        flake-outputs.nixosModules.platform
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
        # Everything every user gets. Aspects are imported per user instead, so
        # two users can run different profiles.
        sharedModules = [
          agenix.homeManagerModules.default
          flake-outputs.homeModules.platform
          editor-program

          { _module.args.host = host; }
        ];
      };
    };
}
