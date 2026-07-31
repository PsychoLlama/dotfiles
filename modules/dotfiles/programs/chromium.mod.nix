{
  cfg,
  lib,
  pkgs,
  ...
}:

{
  options = {
    package = lib.mkPackageOption pkgs.unstable "chromium" { };
  };

  modules.home-manager.programs.chromium = {
    enable = lib.mkDefault true;
    package = lib.mkDefault cfg.package;
  };
}
