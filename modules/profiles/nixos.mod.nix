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

    "${self.presets.programs.sway}".enable = mkDefault true;
    "${self.presets.programs.swaylock}".enable = mkDefault true;
    "${self.presets.programs.waybar}".enable = mkDefault true;
    "${self.presets.programs.zathura}".enable = mkDefault true;

    "${self.presets.services.dunst}".enable = mkDefault true;
    "${self.presets.services.gammastep}".enable = mkDefault true;
    "${self.presets.services.greetd}".enable = mkDefault true;
    "${self.presets.services.pipewire}".enable = mkDefault true;
    "${self.presets.services.swaybg}".enable = mkDefault true;
    "${self.presets.services.swayidle}".enable = mkDefault true;
  };

  # Presets that still live in the old `psychollama.presets.*` namespace.
  # A platform block reaches the host's own options directly, so the two
  # systems can coexist for as long as the migration takes.
  platforms = {
    nixos = {
      psychollama.presets = {
        services = {
          agenix.enable = mkDefault true;
          avahi.enable = mkDefault true;
          podman.enable = mkDefault true;
          restic.enable = mkDefault true;
          syncthing.enable = mkDefault true;
          tailscale.enable = mkDefault true;
          zfs.enable = mkDefault true;
        };

        programs = {
          codex.enable = mkDefault true;
          wireshark.enable = mkDefault true;
        };
      };

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
        bemoji.enable = mkDefault true;
        brightnessctl.enable = mkDefault true;
        chromium.enable = mkDefault true;
        firefox.enable = mkDefault true;
        fuzzel.enable = mkDefault true;
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
