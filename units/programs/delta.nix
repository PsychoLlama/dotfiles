{ units, module, ... }:

let
  inherit (units.platform) home-manager;
in

{
  unit = module.component "unit" {
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
