{ config, lib, ... }:

let
  cfg = config.psychollama.presets.services.pipewire;
in
{
  options.psychollama.presets.services.pipewire = {
    enable = lib.mkEnableOption "Use pipewire for audio";
  };

  config = lib.mkIf cfg.enable {
    # Used by Pipewire to get real-time thread priority.
    security.rtkit.enable = lib.mkDefault true;

    services.pipewire = {
      enable = lib.mkDefault true;
      audio.enable = lib.mkDefault true;
      alsa.enable = lib.mkDefault true;
      pulse.enable = lib.mkDefault true;
    };
  };
}
