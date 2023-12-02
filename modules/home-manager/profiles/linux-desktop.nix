{ config, lib, ... }:

with lib;

let cfg = config.profiles.linux-desktop;

in {
  options.profiles.linux-desktop.enable =
    mkEnableOption "Use an opinionated Linux desktop environment";

  config = mkIf cfg.enable {
    services.presets = {
      dunst.enable = mkDefault true;
      gammastep.enable = mkDefault true;
      sway.enable = mkDefault true;
      swaybg.enable = mkDefault true;
      swayidle.enable = mkDefault true;
    };

    programs = {
      acpi.enable = mkDefault true;
      brightnessctl.enable = mkDefault true;
      grim.enable = mkDefault true;
      pamixer.enable = mkDefault true;
      parted.enable = mkDefault true;
      playerctl.enable = mkDefault true;
      slurp.enable = mkDefault true;
      wf-recorder.enable = mkDefault true;
      wl-clipboard.enable = mkDefault true;

      presets = {
        firefox.enable = mkDefault true;
        rofi.enable = mkDefault true;
        swaylock.enable = mkDefault true;
        waybar.enable = mkDefault true;
        zathura.enable = mkDefault true;
      };
    };
  };
}
