{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.unit "delta" {
  unit = rhizome.component [
    (home-manager.program "delta")

    (home-manager.module ({
      programs.delta = {
        enableGitIntegration = true;

        options = {
          dark = true;
          syntax-theme = "OneHalfDark";
        };
      };
    }))
  ];
}
