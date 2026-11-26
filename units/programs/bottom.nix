{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.unit "bottom" {
  unit = rhizome.component [
    (home-manager.program "bottom")

    (home-manager.module ({ programs.bottom.settings.flags.temperature_type = "f"; }))
  ];
}
