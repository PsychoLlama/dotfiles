{ lib, pkgs, ... }:

{
  modules.home-manager.programs.glow = {
    enable = lib.mkDefault true;
    package = lib.mkDefault pkgs.unstable.glow;

    settings = {
      local = true;
      pager = false;
    };
  };
}
