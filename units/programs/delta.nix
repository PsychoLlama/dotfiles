{ units, define, ... }:

let
  inherit (units.platform) home-manager;
in

{
  unit = define.unit {
    provides = [
      (home-manager.lib.program "delta")

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
  };
}
