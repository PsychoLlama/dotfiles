{ lib, pkgs, ... }:

{
  modules.home-manager =
    { config, ... }:

    let
      swaylock = lib.getExe' config.programs.swaylock.package "swaylock";

      # Sway itself is installed by the OS, so there is no home-manager
      # package to borrow the client from.
      swaymsg = lib.getExe' pkgs.sway "swaymsg";
    in

    {
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
    };
}
