{
  imports = [
    ../programs/acpi.nix
    ../programs/brightnessctl.nix
    ../programs/firefox.nix
    ../programs/grim.nix
    ../programs/pamixer.nix
    ../programs/parted.nix
    ../programs/playerctl.nix
    ../programs/slurp.nix
    ../programs/sway.nix
    ../programs/wf-recorder.nix
    ../programs/wireplumber.nix
    ../programs/wl-clipboard.nix
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
          chromium.enable = mkDefault true;
          bemoji.enable = mkDefault true;
          fuzzel.enable = mkDefault true;
          swaylock.enable = mkDefault true;
          waybar.enable = mkDefault true;
        };
      };
    };
}
