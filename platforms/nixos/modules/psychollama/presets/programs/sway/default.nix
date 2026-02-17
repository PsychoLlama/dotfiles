{ config, lib, ... }:

let
  cfg = config.psychollama.presets.programs.sway;
in

{
  options.psychollama.presets.programs.sway = {
    enable = lib.mkEnableOption "Use SwayWM as the desktop environment";
  };

  config = lib.mkIf cfg.enable {
    programs.sway.enable = lib.mkDefault true;

    # Home-manager generates the real config; suppress the package default.
    environment.etc."sway/config".enable = false;

    # Powers screen capture in Firefox.
    xdg.portal.wlr.enable = true;
  };
}
