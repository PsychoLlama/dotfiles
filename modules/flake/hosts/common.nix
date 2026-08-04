{
  config,
  inputs,
  den,
  ...
}:

let
  inherit (config.flake) modules overlays;
  inherit (inputs)
    agenix
    home-manager
    nixpkgs-unstable
    self
    ;

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
            modules.editor.platform
            modules.editor.configs
            modules.generic.configs

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
  den.schema.host.includes = [
    den.aspects.common
    den.batteries.hostname

    # TODO: Move into respective program aspects once they exist.
    (den.batteries.unfree [
      "claude-code" # symlinkJoin wrapper from home-manager
      "claude-code-bin"
      "spotify"
    ])
  ];

  # Base configuration every host inherits.
  den.aspects.common.nixos =
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
        modules.nixos.configs
        modules.generic.configs
      ];

      # Surface this flake's git revision in `nixos-version --json` so the
      # running system can be traced back to the source commit.
      system.configurationRevision = self.rev or self.dirtyRev or null;

      nixpkgs = {
        overlays = [
          overlays.unstable-packages
          overlays.custom-packages
          overlays.custom-patches
          overlays.vim-plugins
        ];

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

      home-manager = {
        useGlobalPkgs = lib.mkDefault true;
        useUserPackages = lib.mkDefault true;

        # Add custom dotfiles modules to the HM framework.
        sharedModules = [
          agenix.homeManagerModules.default
          modules.homeManager.platform
          modules.homeManager.configs
          modules.generic.configs
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
