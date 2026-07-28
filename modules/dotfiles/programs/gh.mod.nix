{ lib, pkgs, ... }:

{
  modules.home-manager.programs.gh = {
    enable = lib.mkDefault true;
    package = pkgs.unstable.gh;

    settings = {
      git_protocol = "ssh";
    };
  };
}
