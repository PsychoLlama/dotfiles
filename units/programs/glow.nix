{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.unit "glow" {
  unit = rhizome.component [
    (home-manager.program "glow")

    (home-manager.module ({
      programs.glow.settings = {
        local = true;
        pager = false;
      };
    }))
  ];
}
