{ config, lib, ... }:

let
  cfg = config.psychollama.presets.programs.glow;
in

{
  config.programs.glow = lib.mkIf cfg.enable {
    settings = {
      local = true;
      pager = false;
    };
  };
}
