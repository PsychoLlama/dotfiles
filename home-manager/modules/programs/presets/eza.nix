{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.programs.presets.eza;
in
{
  options.programs.presets.eza.enable = mkEnableOption "Replace ls with eza";

  config = mkIf cfg.enable {
    programs.eza = {
      enable = true;
      package = pkgs.unstable.eza;
    };

    home.shellAliases = {
      ls = "eza";
      l = "eza -la";
    };
  };
}
