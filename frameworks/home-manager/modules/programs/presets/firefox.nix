{ config, lib, ... }:

with lib;

let
  cfg = config.programs.presets.firefox;
in
{
  options.programs.presets.firefox.enable = mkEnableOption "Install and configure Firefox";

  config.programs.firefox = mkIf cfg.enable {
    enable = true;

    profiles.default = {
      isDefault = true;
      name = "default";
    };
  };
}
