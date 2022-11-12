{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.sway;

in {
  options.presets.sway.enable = mkEnableOption "Use the Sway window manager";

  # This module is a lie. It approximates the behavior of Sway *if* it were
  # managed by home-manager, allowing swayidle and friends to launch
  # correctly.
  #
  # In an ideal world, I'd be letting home-manager do this.
  config = mkIf cfg.enable {
    systemd.user.targets.sway-session.Unit = {
      Description = "sway compositor session";
      Documentation = [ "man:systemd.special(7)" ];
      BindsTo = [ "graphical-session.target" ];
      Wants = [ "graphical-session-pre.target" ];
      After = [ "graphical-session-pre.target" ];
    };
  };
}
