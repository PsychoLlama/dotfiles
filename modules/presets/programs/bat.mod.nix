{ lib, pkgs, ... }:

{
  platforms.home-manager = {
    home.shellAliases.cat = "bat";

    programs.bat = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.unstable.bat;

      config = {
        theme = "TwoDark";
        style = "changes";
      };
    };
  };
}
