{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.bat;

in {
  options.presets.bat.enable = mkEnableOption "Replace cat with bat";

  config = mkIf cfg.enable {
    home.shellAliases.cat = "bat";

    programs.bat = {
      enable = true;
      config = {
        theme = "TwoDark";
        style = "changes";
      };
    };
  };
}
