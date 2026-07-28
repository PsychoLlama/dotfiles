{
  agenix,
  home-manager,
  nixpkgs,
  nixpkgs-unstable,
  self,
  ...
}:

# Wraps the system builders for NixOS and Home Manager to inject the dotfiles
# framework and provide base configuration.

let
  inherit (nixpkgs) lib;

  manage-system-name = hostName: {
    # The hostname determines the default attrset key to use when rebuilding
    # the system.
    networking.hostName = lib.mkDefault hostName;
  };

  # Surface this flake's git revision in `nixos-version --json` so the
  # running system can be traced back to the source commit.
  configuration-revision = {
    system.configurationRevision = self.rev or self.dirtyRev or null;
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
          class = "editor";
          specialArgs = {
            inherit pkgs;
          };

          modules = [ self.nixosModules.editor-platform ];
        };
      };

      config.home.packages = lib.mkIf cfg.enable [ cfg.neovim ];
    };

  # Routes the `editor` class. `programs.editor` is a submodule of the
  # home-manager eval, so fragments ride `sharedModules` down to it and land
  # as definitions — a submodule definition is a module, so it may carry
  # `imports`. Nothing here touches the option's *type*, which would make the
  # declaration depend on config and recurse.
  editor-installer =
    { config, ... }:
    {
      _meta.routed = [ "editor" ];

      home-manager.sharedModules = [
        { programs.editor.imports = config._meta.fragments.editor; }
      ];
    };

  # Set reasonable defaults for home-manager as a submodule.
  # `theme`, `identity` and `trusted-directories` used to be copied down from
  # the host platform here. They are meta-modules now: consumers read them off
  # `self`, including the editor.
  hm-substrate = {
    home-manager = {
      useGlobalPkgs = lib.mkDefault true;
      useUserPackages = lib.mkDefault true;

      # Add custom dotfiles modules to the HM framework.
      sharedModules = [
        agenix.homeManagerModules.default
        self.nixosModules.home-manager-platform
        editor-program
      ];
    };
  };

in

{
  # The plugins arrive instantiated: mounting one twice with different inputs
  # is an error, so the assembler owns the instances.
  #
  # Type: AttrSet Plugin -> { <hostName> = [ Module ]; } -> AttrSet NixosSystem
  nixos =
    plugins:
    lib.mapAttrs (
      hostName: modules:
      lib.nixosSystem {
        modules = modules ++ [
          agenix.nixosModules.default
          home-manager.nixosModules.home-manager

          # Mounts every meta-module's options into this root's fixpoint and
          # routes home-manager fragments onto `sharedModules`.
          (self.lib.module.roots.nixos plugins)
          editor-installer

          nixpkgs-config
          nix-flakes
          hm-substrate
          configuration-revision

          (manage-system-name hostName)
        ];
      }
    );
}
