{
  # Consumed by other flakes: importing this picks the profile, and its
  # presets land in `self.modules.<class>.default`.
  flake.modules.flake.linux-desktop = ./linux-desktop.nix;

  imports = [
    ../aspects/programs/acpi.nix
    ../aspects/programs/bemoji.nix
    ../aspects/programs/brightnessctl.nix
    ../aspects/programs/chromium.nix
    ../aspects/programs/firefox.nix
    ../aspects/programs/fuzzel.nix
    ../aspects/programs/grim.nix
    ../aspects/programs/pamixer.nix
    ../aspects/programs/parted.nix
    ../aspects/programs/playerctl.nix
    ../aspects/programs/slurp.nix
    ../aspects/programs/sway.nix
    ../aspects/programs/swaylock.nix
    ../aspects/programs/waybar
    ../aspects/programs/wf-recorder.nix
    ../aspects/programs/wireplumber.nix
    ../aspects/programs/wl-clipboard.nix
    ../aspects/services/dunst.nix
    ../aspects/services/gammastep.nix
    ../aspects/services/network-manager.nix
    ../aspects/services/swaybg.nix
    ../aspects/services/swayidle.nix
    ../aspects/system/fonts.nix
    ../aspects/system/gtk.nix
    ../aspects/system/sound-theme.nix

    # Enabled directly below, without a preset of its own.
    ../platform/homeManager/programs/signal-desktop.nix
  ];

  flake.modules.homeManager.default =
    { lib, ... }:

    {
      programs.signal-desktop.enable = lib.mkDefault true;
    };
}
