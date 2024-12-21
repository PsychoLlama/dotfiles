{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.presets.programs.bat;
in
{
  options.presets.programs.bat.enable = mkEnableOption "Replace cat with bat";

  config = mkIf cfg.enable {
    home.shellAliases.cat = "bat";

    programs.bat = {
      enable = true;
      package = pkgs.unstable.bat;
      config = {
        theme = "TwoDark";
        style = "changes";
      };
    };
  };
}
