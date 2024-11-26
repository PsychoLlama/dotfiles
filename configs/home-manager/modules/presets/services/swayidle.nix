{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.presets.services.swayidle;
  swaylock = "${pkgs.swaylock}/bin/swaylock";
  swaymsg = "${pkgs.sway}/bin/swaymsg";
in
{
  options.presets.services.swayidle.enable = mkEnableOption "Automatically lock the computer when inactive";

  config.services.swayidle = mkIf cfg.enable {
    enable = true;

    events = [
      {
        event = "before-sleep";
        command = swaylock;
      }
    ];

    # Lock the screen after 15 minutes of inactivity, then turn off the
    # displays after another 2 minutes, and turn back on when resumed.
    timeouts = [
      {
        timeout = 900;
        command = swaylock;
      }
      {
        timeout = 1020;
        command = "${swaymsg} 'output * dpms off'";
        resumeCommand = "${swaymsg} 'output * dpms on'";
      }
    ];
  };
}
