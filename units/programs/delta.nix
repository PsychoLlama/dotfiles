{ units, define, ... }:

let
  inherit (units.platform) home-manager;
in

{
  unit = define.unit {
    config = [
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
