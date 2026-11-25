{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.component "delta" [
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
]
