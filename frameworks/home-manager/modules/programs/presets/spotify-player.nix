{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.presets.spotify-player;
in
{
  options.programs.presets.spotify-player.enable = lib.mkEnableOption "Install and configure spotify-player";

  config.programs.spotify-player = lib.mkIf cfg.enable {
    enable = true;
    package = pkgs.unstable.spotify-player;
    settings = {
      enable_notify = false;
      device.volume = 100;
    };
  };
}
