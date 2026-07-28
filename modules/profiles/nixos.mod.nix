{ lib, ... }:

# Everything a NixOS desktop should have. Absorbs what used to be split
# between the NixOS `full` profile and the home-manager `linux-desktop`
# profile — one enable, both platforms.

let
  inherit (lib) mkDefault;
in

{
  # `config` is this plugin's own namespace; the mount point is implied. A typo
  # here is an unknown-option error from the merge machinery, not a
  # silently-ignored option path.
  config.presets = {
    fonts.enable = true;
    gtk.enable = mkDefault true;
    sound-theme.enable = mkDefault true;

    programs = {
      acpi.enable = mkDefault true;
      bemoji.enable = mkDefault true;
      brightnessctl.enable = mkDefault true;
      chromium.enable = mkDefault true;
      codex.enable = mkDefault true;
      firefox.enable = mkDefault true;
      fuzzel.enable = mkDefault true;
      grim.enable = mkDefault true;
      pamixer.enable = mkDefault true;
      parted.enable = mkDefault true;
      playerctl.enable = mkDefault true;
      slurp.enable = mkDefault true;
      sway.enable = mkDefault true;
      swaylock.enable = mkDefault true;
      waybar.enable = mkDefault true;
      wf-recorder.enable = mkDefault true;
      wireplumber.enable = mkDefault true;
      wireshark.enable = mkDefault true;
      wl-clipboard.enable = mkDefault true;
      zathura.enable = mkDefault true;
    };

    services = {
      agenix.enable = mkDefault true;
      avahi.enable = mkDefault true;
      dunst.enable = mkDefault true;
      gammastep.enable = mkDefault true;
      greetd.enable = mkDefault true;
      pipewire.enable = mkDefault true;
      podman.enable = mkDefault true;
      restic.enable = mkDefault true;
      swaybg.enable = mkDefault true;
      swayidle.enable = mkDefault true;
      syncthing.enable = mkDefault true;
      tailscale.enable = mkDefault true;
      zfs.enable = mkDefault true;
    };
  };

  # Host settings small enough that they never earned a preset of their own. A
  # class block reaches that host's own options directly.
  modules = {
    nixos = {
      services = {
        automatic-timezoned.enable = mkDefault true;
        printing.enable = mkDefault true;
      };

      # Build the apropos/whatis cache so `man -k` works. Carapace's native `man`
      # completer shells out to apropos, which returns nothing without it.
      documentation.man.cache.enable = mkDefault true;

      fonts.enableDefaultPackages = mkDefault true;
    };

    home-manager.programs.signal-desktop.enable = mkDefault true;
  };
}
