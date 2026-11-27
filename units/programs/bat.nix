{ units, define, ... }:

let
  inherit (units.platform) home-manager;
in

{
  unit = define.unit {
    config = [
      (home-manager.lib.program "bat")

      (home-manager.module {
        home.shellAliases.cat = "bat";

        programs.bat.config = {
          theme = "TwoDark";
          style = "changes";
        };
      })
    ];
  };
}
