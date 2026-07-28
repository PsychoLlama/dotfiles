{ lib, pkgs, ... }:

{
  modules.home-manager.programs.chromium = {
    enable = lib.mkDefault true;
    package = lib.mkDefault pkgs.unstable.chromium;
  };
}
