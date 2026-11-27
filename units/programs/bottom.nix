{ units, module, ... }:

let
  inherit (units.platform) home-manager;
in

{
  unit = module.component "unit" {
    config = [
      (home-manager.lib.program "bottom")

      (home-manager.module ({ programs.bottom.settings.flags.temperature_type = "f"; }))
    ];
  };
}
