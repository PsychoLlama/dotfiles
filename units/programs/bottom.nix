{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.component "bottom" [
  (home-manager.program "bottom")

  (home-manager.module ({ programs.bottom.settings.flags.temperature_type = "f"; }))
]
