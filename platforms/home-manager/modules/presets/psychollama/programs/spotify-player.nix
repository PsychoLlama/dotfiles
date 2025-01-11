{ config, lib, ... }:

let
  cfg = config.psychollama.presets.programs.spotify-player;
in

{
  config.programs.spotify-player = lib.mkIf cfg.enable {
    settings = {
      enable_notify = false;
      device.volume = 100;
    };
  };
}
