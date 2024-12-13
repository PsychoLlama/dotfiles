{ config, lib, ... }:

let
  cfg = config.profiles.full;
in
{
  options.profiles.full.enable = lib.mkEnableOption "Enable all NixOS presets";

  config = lib.mkIf cfg.enable {
    presets = {
      services = {
        pipewire.enable = lib.mkDefault true;
        greetd.enable = lib.mkDefault true;
      };

      programs = {
        hyprland.enable = lib.mkDefault true;
        sway.enable = lib.mkDefault true;
      };
    };

    services = {
      printing.enable = lib.mkDefault true;
    };
  };
}
