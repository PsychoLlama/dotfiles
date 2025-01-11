{ config, lib, ... }:

let
  cfg = config.presets.programs.firefox;
in

{
  config.programs.firefox = lib.mkIf cfg.enable {
    profiles.default = {
      isDefault = true;
      name = "default";
    };
  };
}
