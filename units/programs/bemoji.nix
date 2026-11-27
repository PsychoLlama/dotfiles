{ units, define, ... }:

let
  inherit (units.platform) home-manager;
in

{
  unit = define.unit {
    provides = [
      (home-manager.module (
        { pkgs, ... }:

        {
          home.packages = [
            pkgs.unstable.bemoji

            # bemoji uses wtype to type the selected emoji into the focused window.
            pkgs.unstable.wtype
          ];

          home.sessionVariables.BEMOJI_PICKER_CMD = "fuzzel -d";
        }
      ))
    ];
  };
}
