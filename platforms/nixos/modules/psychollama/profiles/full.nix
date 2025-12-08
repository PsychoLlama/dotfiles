{ config, lib, ... }:

let
  cfg = config.psychollama.profiles.full;
in

{
  options.psychollama.profiles.full = {
    enable = lib.mkEnableOption "Enable all NixOS presets";
  };

  config = lib.mkIf cfg.enable {
    psychollama.presets = {
      services = {
        greetd.enable = lib.mkDefault true;
        pipewire.enable = lib.mkDefault true;
        podman.enable = lib.mkDefault true;
        syncthing.enable = lib.mkDefault true;
        zfs.enable = lib.mkDefault true;
      };

      programs = {
        npm.enable = lib.mkDefault true;
        sway.enable = lib.mkDefault true;
        wireshark.enable = lib.mkDefault true;
      };
    };

    services = {
      automatic-timezoned.enable = lib.mkDefault true;
      printing.enable = lib.mkDefault true;
      tailscale.enable = lib.mkDefault true;
    };

    fonts.enableDefaultPackages = lib.mkDefault true;
  };
}
