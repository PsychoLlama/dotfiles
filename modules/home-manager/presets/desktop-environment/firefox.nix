{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.firefox;

in {
  options.presets.firefox.enable =
    mkEnableOption "Install and configure Firefox";

  config = mkIf cfg.enable {
    programs.firefox = {
      enable = true;

      profiles.default = {
        isDefault = true;
        name = "default";
      };
    };
  };
}
