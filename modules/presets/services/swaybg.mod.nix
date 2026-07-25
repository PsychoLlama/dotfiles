{ pkgs, ... }:

{
  platforms.home-manager.services.swaybg = {
    enable = true;
    package = pkgs.unstable.swaybg;
    image = "attic/images/wallpapers/current";
  };
}
