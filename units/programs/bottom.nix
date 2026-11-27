{ units, define, ... }:

let
  inherit (units.platform) home-manager;
in

{
  unit = define.unit {
    provides = [
      (home-manager.lib.program "bottom")

      (home-manager.module ({ programs.bottom.settings.flags.temperature_type = "f"; }))
    ];
  };
}
