{ config, lib, ... }:

let
  cfg = config.psychollama.presets.programs.firefox;
in

{
  config.programs.firefox = lib.mkIf cfg.enable {
    profiles.default = {
      isDefault = true;
      name = "default";
    };
  };
}
