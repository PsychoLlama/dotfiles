{ config, lib, ... }:

with lib;

let
  cfg = config.presets.programs.firefox;
in
{
  options.presets.programs.firefox.enable = mkEnableOption "Install and configure Firefox";

  config.programs.firefox = mkIf cfg.enable {
    enable = true;

    profiles.default = {
      isDefault = true;
      name = "default";
    };
  };
}
