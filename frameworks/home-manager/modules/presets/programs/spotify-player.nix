{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.presets.programs.spotify-player;
in
{
  options.presets.programs.spotify-player.enable = lib.mkEnableOption "Install and configure spotify-player";

  config.programs.spotify-player = lib.mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.spotify-player;
    settings = {
      enable_notify = false;
      device.volume = 100;
    };
  };
}
