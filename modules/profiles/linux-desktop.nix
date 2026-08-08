{
  # Consumed by other flakes: importing this picks the profile, and its
  # presets land in `self.modules.<class>.default`.
  flake.modules.flake.linux-desktop = ./linux-desktop.nix;

  imports = [
    ../programs/acpi.nix
    ../programs/bemoji.nix
    ../programs/brightnessctl.nix
    ../programs/chromium.nix
    ../programs/firefox.nix
    ../programs/fuzzel.nix
    ../programs/grim.nix
    ../programs/pamixer.nix
    ../programs/parted.nix
    ../programs/playerctl.nix
    ../programs/slurp.nix
    ../programs/sway.nix
    ../programs/swaylock.nix
    ../programs/waybar
    ../programs/wf-recorder.nix
    ../programs/wireplumber.nix
    ../programs/wl-clipboard.nix
    ../services/dunst.nix
    ../services/gammastep.nix
    ../services/network-manager.nix
    ../services/swaybg.nix
    ../services/swayidle.nix
    ../system/fonts.nix
    ../system/gtk.nix
    ../system/sound-theme.nix

    # Enabled directly below, without a preset of its own.
    ../extensions/programs/signal-desktop.nix
  ];

  flake.modules.homeManager.default =
    { lib, ... }:

    {
      programs.signal-desktop.enable = lib.mkDefault true;
    };
}
