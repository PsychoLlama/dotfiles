{ lib, pkgs, ... }:

{
  platforms.home-manager.programs.chromium = {
    enable = lib.mkDefault true;
    package = lib.mkDefault pkgs.unstable.chromium;
  };
}
