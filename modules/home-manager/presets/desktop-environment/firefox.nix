{ config, lib, pkgs, ... }:

let cfg = config.presets.firefox;

in with lib; {
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
