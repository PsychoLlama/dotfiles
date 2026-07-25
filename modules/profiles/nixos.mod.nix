{ self, lib, ... }:

# Everything a NixOS desktop should have. Absorbs what used to be split
# between the NixOS `full` profile and the home-manager `linux-desktop`
# profile — one enable, both platforms.

let
  inherit (lib) mkDefault;
in

{
  # Writes addressed by handle. A typo here is an eval error at the
  # reference, not a silently-ignored option path.
  config = {
    "${self.presets.fonts}".enable = true;
    "${self.presets.gtk}".enable = mkDefault true;
    "${self.presets.sound-theme}".enable = mkDefault true;

    "${self.presets.programs.bemoji}".enable = mkDefault true;
    "${self.presets.programs.chromium}".enable = mkDefault true;
    "${self.presets.programs.codex}".enable = mkDefault true;
    "${self.presets.programs.firefox}".enable = mkDefault true;
    "${self.presets.programs.fuzzel}".enable = mkDefault true;
    "${self.presets.programs.sway}".enable = mkDefault true;
    "${self.presets.programs.swaylock}".enable = mkDefault true;
    "${self.presets.programs.waybar}".enable = mkDefault true;
    "${self.presets.programs.wireshark}".enable = mkDefault true;
    "${self.presets.programs.zathura}".enable = mkDefault true;

    "${self.presets.services.agenix}".enable = mkDefault true;
    "${self.presets.services.avahi}".enable = mkDefault true;
    "${self.presets.services.dunst}".enable = mkDefault true;
    "${self.presets.services.gammastep}".enable = mkDefault true;
    "${self.presets.services.greetd}".enable = mkDefault true;
    "${self.presets.services.pipewire}".enable = mkDefault true;
    "${self.presets.services.podman}".enable = mkDefault true;
    "${self.presets.services.restic}".enable = mkDefault true;
    "${self.presets.services.swaybg}".enable = mkDefault true;
    "${self.presets.services.swayidle}".enable = mkDefault true;
    "${self.presets.services.syncthing}".enable = mkDefault true;
    "${self.presets.services.tailscale}".enable = mkDefault true;
    "${self.presets.services.zfs}".enable = mkDefault true;
  };

  # Host settings small enough that they never earned a preset of their own,
  # plus (under home-manager) presets that still live in the old
  # `psychollama.presets.*` namespace. A platform block reaches the host's own
  # options directly, so the two systems can coexist for as long as the
  # migration takes.
  platforms = {
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

    home-manager = {
      programs.signal-desktop.enable = mkDefault true;

      psychollama.presets.programs = {
        acpi.enable = mkDefault true;
        brightnessctl.enable = mkDefault true;
        grim.enable = mkDefault true;
        pamixer.enable = mkDefault true;
        parted.enable = mkDefault true;
        playerctl.enable = mkDefault true;
        slurp.enable = mkDefault true;
        wf-recorder.enable = mkDefault true;
        wireplumber.enable = mkDefault true;
        wl-clipboard.enable = mkDefault true;
      };
    };
  };
}
