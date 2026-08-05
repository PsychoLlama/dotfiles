{
  dotfiles.services.swayidle.homeManager =
    {
      config,
      pkgs,
      lib,
      ...
    }:

    let
      swaylock = lib.getExe' config.programs.swaylock.package "swaylock";
      swaymsg = "${pkgs.sway}/bin/swaymsg";
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
