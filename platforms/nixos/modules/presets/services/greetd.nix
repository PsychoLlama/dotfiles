{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.presets.services.greetd;
in
{
  options.presets.services.greetd.enable = mkEnableOption "Use TUI greeter";

  config = mkIf cfg.enable {
    services.greetd = {
      enable = true;
      settings.default_session = {
        user = "greeter";
        command = "${lib.makeBinPath [ pkgs.unstable.greetd.tuigreet ]}/tuigreet --asterisks -trc Hyprland";
      };
    };

    # Avoids interleaving with systemd output.
    systemd.services.greetd.serviceConfig.Type = "idle";
    services.greetd.vt = 2;
  };
}
