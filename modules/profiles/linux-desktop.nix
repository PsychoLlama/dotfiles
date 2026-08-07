{
  imports = [
    ../programs/sway.nix
    ../services/swaybg.nix
    ../system/fonts.nix
  ];

  flake.modules.homeManager.default =
    { lib, ... }:

    let
      inherit (lib) mkDefault;
    in

    {
      programs = {
        signal-desktop.enable = mkDefault true;
      };

      psychollama.presets = {
        gtk.enable = mkDefault true;
        sound-theme.enable = mkDefault true;

        services = {
          dunst.enable = mkDefault true;
          gammastep.enable = mkDefault true;
          swayidle.enable = mkDefault true;
        };

        programs = {
          acpi.enable = mkDefault true;
          brightnessctl.enable = mkDefault true;
          chromium.enable = mkDefault true;
          bemoji.enable = mkDefault true;
          firefox.enable = mkDefault true;
          fuzzel.enable = mkDefault true;
          grim.enable = mkDefault true;
          pamixer.enable = mkDefault true;
          parted.enable = mkDefault true;
          playerctl.enable = mkDefault true;
          slurp.enable = mkDefault true;
          swaylock.enable = mkDefault true;
          waybar.enable = mkDefault true;
          wf-recorder.enable = mkDefault true;
          wireplumber.enable = mkDefault true;
          wl-clipboard.enable = mkDefault true;
        };
      };
    };
}
