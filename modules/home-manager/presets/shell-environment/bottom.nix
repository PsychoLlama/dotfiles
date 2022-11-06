{ config, lib, ... }:

let cfg = config.presets.bottom;

in with lib; {
  options.presets.bottom.enable =
    mkEnableOption "Use and configure the bottom process monitor";

  config.programs.bottom = mkIf cfg.enable {
    enable = true;
    settings.flags.temperature_type = "f";
  };
}
