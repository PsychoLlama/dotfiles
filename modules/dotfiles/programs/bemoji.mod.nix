{ pkgs, ... }:

{
  modules.home-manager = {
    programs = {
      bemoji = {
        enable = true;
        package = pkgs.unstable.bemoji;
      };

      # bemoji uses wtype to type the selected emoji into the focused window.
      wtype = {
        enable = true;
        package = pkgs.unstable.wtype;
      };
    };

    home.sessionVariables.BEMOJI_PICKER_CMD = "fuzzel -d";
  };
}
