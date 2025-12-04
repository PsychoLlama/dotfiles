{ config, lib, ... }:

let
  inherit (lib) mkDefault;
  cfg = config.psychollama.profiles.linux-desktop;
in

{
  options.psychollama.profiles.linux-desktop = {
    enable = lib.mkEnableOption "Enable all Linux desktop presets";
  };

  config = lib.mkIf cfg.enable {
    programs = {
      signal-desktop.enable = mkDefault true;
    };

    psychollama.presets = {
      fonts.enable = true;

      services = {
        dunst.enable = mkDefault true;
        gammastep.enable = mkDefault true;
        sway.enable = mkDefault true;
        swaybg.enable = mkDefault true;
        swayidle.enable = mkDefault true;
      };

      programs = {
        acpi.enable = mkDefault true;
        brightnessctl.enable = mkDefault true;
        chromium.enable = mkDefault true;
        firefox.enable = mkDefault true;
        fuzzel.enable = mkDefault true;
        grim.enable = mkDefault true;
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
