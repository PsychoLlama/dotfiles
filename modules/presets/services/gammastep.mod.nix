{ pkgs, ... }:

{
  platforms.home-manager.services.gammastep = {
    enable = true;
    package = pkgs.unstable.gammastep;
    dawnTime = "6:30-7:00";
    duskTime = "21:30-22:00";

    # home-manager defaults to 5500/3700. Restore gammastep's own defaults:
    # 6500K is the neutral point, so daytime leaves the display untouched.
    temperature = {
      day = 6500;
      night = 4500;
    };
  };
}
