{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.component "bemoji" [
  (home-manager.module (
    { pkgs, ... }:

    {
      programs.bemoji = {
        enable = true;
        package = pkgs.unstable.bemoji;
      };

      # bemoji uses wtype to type the selected emoji into the focused window.
      programs.wtype = {
        enable = true;
        package = pkgs.unstable.wtype;
      };

      home.sessionVariables.BEMOJI_PICKER_CMD = "fuzzel -d";
    }
  ))
]
