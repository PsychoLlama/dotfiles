{ config, ... }:

let
  editorModules = config.flake.editorModules;
in

{
  flake.homeModules.platform =
    {
      config,
      host,
      lib,
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
            editorModules.platform

            { _module.args.host = host; }
          ];
        };
      };

      config.home.packages = lib.mkIf cfg.enable [ cfg.neovim ];
    };
}
