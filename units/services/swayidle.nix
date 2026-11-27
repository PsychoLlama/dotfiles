{
  units,
  self,
  module,
  ...
}:

let
  inherit (units.platform) home-manager;
in

{
  # While this file exists, swayidle will not start. Persists the idle
  # inhibitor setting between NixOS activations.
  lib.disabled-flag = username: "/tmp/swayidle/${username}.disabled";

  unit = module.component {
    config = [
      (home-manager.module (
        {
          config,
          lib,
          pkgs,
          ...
        }:

        let
          swaylock = lib.getExe' config.programs.swaylock.package "swaylock";
          swaymsg = "${pkgs.sway}/bin/swaymsg";
        in

        {
          systemd.user.services.swayidle.Unit.ConditionPathExists =
            "!${self.lib.disabled-flag config.home.username}";

          services.swayidle = {
            enable = true;

            events.before-sleep = swaylock;

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
      ))
    ];
  };
}
