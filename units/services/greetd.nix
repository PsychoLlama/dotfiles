{ units, define, ... }:

let
  inherit (units.platform) nixos;
in

{
  unit = define.unit {
    provides = [
      (nixos.module (
        {
          config,
          lib,
          pkgs,
          ...
        }:

        {
          services.greetd = {
            enable = true;
            settings.default_session = {
              user = "greeter";
              command = "${lib.makeBinPath [ pkgs.unstable.tuigreet ]}/tuigreet --asterisks -trc sway";
            };
          };

          # Avoids interleaving with systemd output.
          systemd.services.greetd.serviceConfig.Type = "idle";
        }
      ))
    ];
  };
}
