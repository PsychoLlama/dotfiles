{
  exports.homeManager =
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
      options.psychollama.presets.services.swayidle = {
        # Used to persist the idle inhibitor setting between NixOS activations.
        path-condition = lib.mkOption {
          type = lib.types.str;
          readOnly = true;
          default = "/tmp/swayidle/${config.home.username}.disabled";
          description = "Path that keeps swayidle from starting while it exists.";
        };
      };

      config = {
        systemd.user.services.swayidle.Unit.ConditionPathExists =
          "!${config.psychollama.presets.services.swayidle.path-condition}";

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
    };
}
