{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.unit "bat" {
  unit = rhizome.component [
    (home-manager.program "bat")

    (home-manager.module {
      home.shellAliases.cat = "bat";

      programs.bat.config = {
        theme = "TwoDark";
        style = "changes";
      };
    })
  ];
}
