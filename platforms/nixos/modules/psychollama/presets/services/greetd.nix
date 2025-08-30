{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.services.greetd;
in

{
  options.psychollama.presets.services.greetd.enable = lib.mkEnableOption "Use TUI greeter";

  config = lib.mkIf cfg.enable {
    services.greetd = {
      enable = true;
      settings.default_session = {
        user = "greeter";
        command = "${lib.makeBinPath [ pkgs.unstable.tuigreet ]}/tuigreet --asterisks -trc sway";
      };
    };

    # Avoids interleaving with systemd output.
    systemd.services.greetd.serviceConfig.Type = "idle";
    services.greetd.vt = 2;
  };
}
