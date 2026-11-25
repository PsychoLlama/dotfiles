{ rhizome, units, ... }:

let
  inherit (units.platform) nixos;
in

rhizome.component "pipewire" [
  (nixos.module (
    { config, lib, ... }:

    {
      # Used by Pipewire to get real-time thread priority.
      security.rtkit.enable = lib.mkDefault true;

      services.pipewire = {
        enable = lib.mkDefault true;
        audio.enable = lib.mkDefault true;
        alsa.enable = lib.mkDefault true;
        pulse.enable = lib.mkDefault true;
      };
    }
  ))
]
