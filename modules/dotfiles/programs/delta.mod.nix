{ lib, pkgs, ... }:

{
  modules.home-manager.programs.delta = {
    enable = lib.mkDefault true;
    package = lib.mkDefault pkgs.unstable.delta;

    enableGitIntegration = true;

    options = {
      dark = true;
      syntax-theme = "OneHalfDark";
    };
  };
}
