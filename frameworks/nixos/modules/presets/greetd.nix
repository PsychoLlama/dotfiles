{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.dotfiles.presets.greetd;
in
{
  options.dotfiles.presets.greetd.enable = mkEnableOption "Use recommended fonts";

  config = mkIf cfg.enable {
    services.greetd = {
      enable = true;
      settings.default_session = {
        user = "greeter";
        command = "${lib.makeBinPath [ pkgs.unstable.greetd.tuigreet ]}/tuigreet --asterisks -trc sway";
      };
    };

    # Avoids interleaving with systemd output.
    systemd.services.greetd.serviceConfig.Type = "idle";
    services.greetd.vt = 2;
  };
}
