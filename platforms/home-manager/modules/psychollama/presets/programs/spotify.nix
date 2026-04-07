{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.spotify;
in

{
  options.psychollama.presets.programs.spotify = {
    enable = lib.mkEnableOption "Install the Spotify desktop client";
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ pkgs.unstable.spotify ];
  };
}
