{ config, den, ... }:

let
  inherit (config.flake) modules;

  # Declares `programs.editor`, the option the class nests into.
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
  den.classes.editor.description = "Neovim configuration, nested into home-manager's `programs.editor`";

  # Declares the option on every user. `policy.provide` resolves once and would
  # only reach the first.
  den.aspects.editor-class.homeManager.imports = [ editor-program ];

  den.policies.editor-to-home-manager = _: [
    (den.lib.policy.route {
      fromClass = "editor";
      intoClass = "homeManager";
      path = [
        "programs"
        "editor"
      ];
    })
  ];

  den.schema.user.includes = [
    den.aspects.editor-class
    den.policies.editor-to-home-manager
  ];

  den.schema.home.includes = [
    den.aspects.editor-class
    den.policies.editor-to-home-manager
  ];
}
