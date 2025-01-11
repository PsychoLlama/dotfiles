{ config, lib, ... }:

let
  inherit (lib) mkDefault;
  cfg = config.profiles.linux-desktop;
in

{
  options.profiles.linux-desktop = {
    enable = lib.mkEnableOption "Enable all Linux desktop presets";
  };

  config = lib.mkIf cfg.enable {
    presets = {
      services = {
        dunst.enable = mkDefault true;
        gammastep.enable = mkDefault true;
        hypridle.enable = mkDefault true;
        sway.enable = mkDefault true;
        swaybg.enable = mkDefault true;
      };

      programs = {
        acpi.enable = mkDefault true;
        brightnessctl.enable = mkDefault true;
        firefox.enable = mkDefault true;
        grim.enable = mkDefault true;
        hyprland.enable = mkDefault true;
        hyprlock.enable = mkDefault true;
        pamixer.enable = mkDefault true;
        parted.enable = mkDefault true;
        playerctl.enable = mkDefault true;
        rofi.enable = mkDefault true;
        slurp.enable = mkDefault true;
        swaylock.enable = mkDefault true;
        waybar.enable = mkDefault true;
        wf-recorder.enable = mkDefault true;
        wireplumber.enable = mkDefault true;
        wl-clipboard.enable = mkDefault true;
        zathura.enable = mkDefault true;
      };
    };
  };
}
