{
  # Consumed by other flakes: importing this picks the profile, and its
  # presets land in `self.modules.<class>.default`.
  flake.modules.flake.full = ./full.nix;

  imports = [
    ../aspects/programs/bat.nix
    ../aspects/programs/bottom.nix
    ../aspects/programs/carapace.nix
    ../aspects/programs/claude-code
    ../aspects/programs/codex
    ../aspects/programs/delta.nix
    ../aspects/programs/dictation.nix
    ../aspects/programs/direnv.nix
    ../aspects/programs/dive.nix
    ../aspects/programs/dix.nix
    ../aspects/programs/doggo.nix
    ../aspects/programs/editor.nix
    ../aspects/programs/fd.nix
    ../aspects/programs/fzf.nix
    ../aspects/programs/gh.nix
    ../aspects/programs/git.nix
    ../aspects/programs/glow.nix
    ../aspects/programs/jq.nix
    ../aspects/programs/miniserve.nix
    ../aspects/programs/nix-output-monitor.nix
    ../aspects/programs/nushell
    ../aspects/programs/nushell/swizzle.nix
    ../aspects/programs/python3.nix
    ../aspects/programs/spotify.nix
    ../aspects/programs/starship.nix
    ../aspects/programs/sway.nix
    ../aspects/programs/termshark.nix
    ../aspects/programs/tmux
    ../aspects/programs/viddy.nix
    ../aspects/programs/wezterm.nix
    ../aspects/programs/whois.nix
    ../aspects/programs/wireshark.nix
    ../aspects/programs/zoxide.nix
    ../aspects/services/agenix.nix
    ../aspects/services/avahi.nix
    ../aspects/services/greetd.nix
    ../aspects/services/pipewire.nix
    ../aspects/services/podman.nix
    ../aspects/services/restic
    ../aspects/services/ssh-agent.nix
    ../aspects/services/syncthing.nix
    ../aspects/services/tailscale.nix
    ../aspects/services/zfs.nix
    ../aspects/system/fonts.nix

    # Programs enabled directly below, without a preset of their own.
    ../platform/homeManager/programs/binutils.nix
    ../platform/homeManager/programs/duf.nix
    ../platform/homeManager/programs/hexyl.nix
    ../platform/homeManager/programs/lsof.nix
    ../platform/homeManager/programs/onefetch.nix
    ../platform/homeManager/programs/parted.nix
    ../platform/homeManager/programs/rage.nix
    ../platform/homeManager/programs/tokei.nix
    ../platform/homeManager/programs/viu.nix
  ];

  flake.modules = {
    nixos.default =
      { lib, ... }:

      let
        inherit (lib) mkDefault;
      in

      {
        services = {
          automatic-timezoned.enable = mkDefault true;
          printing.enable = mkDefault true;
        };

        # Build the apropos/whatis cache so `man -k` works. Carapace's native `man`
        # completer shells out to apropos, which returns nothing without it.
        documentation.man.cache.enable = mkDefault true;
      };

    homeManager.default =
      { lib, pkgs, ... }:

      let
        inherit (lib) mkDefault;
      in

      {
        # The Linux man pages aren't a program, so there's no `enable` to reach
        # for. They're what makes section 2 and 3 lookups resolve.
        home.packages = [ pkgs.man-pages ];

        programs = {
          binutils.enable = mkDefault true;
          duf.enable = mkDefault true;
          hexyl.enable = mkDefault true;
          lsof.enable = mkDefault true;
          man.generateCaches = mkDefault true;
          nh.enable = mkDefault true;
          onefetch.enable = mkDefault true;
          parted.enable = mkDefault pkgs.stdenv.isLinux;
          rage.enable = mkDefault true;
          ripgrep.enable = mkDefault true;
          tokei.enable = mkDefault true;
          viu.enable = mkDefault true;
        };
      };
  };
}
