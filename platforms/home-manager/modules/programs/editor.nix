{
  lib,
  config,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.programs.editor;
in
{
  options.programs.editor = mkOption {
    type = types.submoduleWith {
      modules = [ ../../../editor/modules ];
      specialArgs = {
        inherit pkgs;
      };
    };

    description = "Configure and install the Neovim editor";
    default = { };
  };

  config.home.packages = mkIf cfg.enable [ cfg.neovim ];
}
