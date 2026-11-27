{ units, define, ... }:

let
  inherit (units.platform) nixos;
in

{
  unit = define.unit {
    config = [
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
    ];
  };
}
