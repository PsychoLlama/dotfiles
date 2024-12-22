{ config, lib, ... }:

let
  cfg = config.presets.programs.glow;
in

{
  config.programs.glow = lib.mkIf cfg.enable {
    settings = {
      local = true;
      pager = false;
    };
  };
}
