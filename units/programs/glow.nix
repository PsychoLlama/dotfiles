{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.component "glow" [
  (home-manager.program "glow")

  (home-manager.module ({
    programs.glow.settings = {
      local = true;
      pager = false;
    };
  }))
]
