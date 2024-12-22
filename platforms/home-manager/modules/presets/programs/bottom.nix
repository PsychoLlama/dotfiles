{ config, lib, ... }:

let
  cfg = config.presets.programs.bottom;
in

{
  config.programs.bottom = lib.mkIf cfg.enable {
    settings.flags.temperature_type = "f";
  };
}
