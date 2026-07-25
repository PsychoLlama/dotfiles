{ lib, pkgs, ... }:

{
  platforms.home-manager.programs.bottom = {
    enable = lib.mkDefault true;
    package = lib.mkDefault pkgs.unstable.bottom;

    settings.flags.temperature_type = "f";
  };
}
