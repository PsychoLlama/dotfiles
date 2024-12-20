{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.presets.programs.glow;
in
{
  options.presets.programs.glow.enable = mkEnableOption "Use an opinionated install of the Glow markdown viewer";

  config = mkIf cfg.enable {
    programs.glow = {
      enable = true;
      package = pkgs.unstable.glow;

      settings = {
        local = true;
        pager = false;
      };
    };
  };
}
