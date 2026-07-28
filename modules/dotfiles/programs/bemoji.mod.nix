{
  cfg,
  lib,
  pkgs,
  ...
}:

{
  options = {
    package = lib.mkPackageOption pkgs.unstable "bemoji" { };
  };

  # bemoji types the selected emoji into the focused window through wtype.
  config.programs.wtype.enable = true;

  modules.home-manager = {
    home.packages = [ cfg.package ];
    home.sessionVariables.BEMOJI_PICKER_CMD = "fuzzel -d";
  };
}
