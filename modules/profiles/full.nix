{
  imports = [
    ../programs/bat.nix
    ../programs/bottom.nix
    ../programs/claude-code
    ../programs/delta.nix
    ../programs/direnv.nix
    ../programs/dive.nix
    ../programs/dix.nix
    ../programs/doggo.nix
    ../programs/fd.nix
    ../programs/fzf.nix
    ../programs/git.nix
    ../programs/glow.nix
    ../programs/jq.nix
    ../programs/miniserve.nix
    ../programs/nix-output-monitor.nix
    ../programs/nushell
    ../programs/nushell/swizzle.nix
    ../programs/python3.nix
    ../programs/starship.nix
    ../programs/sway.nix
    ../programs/termshark.nix
    ../programs/tmux
    ../programs/viddy.nix
    ../programs/wezterm.nix
    ../programs/whois.nix
    ../programs/zoxide.nix
    ../system/fonts.nix
  ];

  flake.modules = {
    nixos.default =
      { lib, ... }:

      let
        inherit (lib) mkDefault;
      in

      {
        psychollama.presets = {
          services = {
            agenix.enable = mkDefault true;
            avahi.enable = mkDefault true;
            greetd.enable = mkDefault true;
            pipewire.enable = mkDefault true;
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
      };

    homeManager.default =
      { lib, pkgs, ... }:

      let
        inherit (lib) mkDefault;
      in

      {
        psychollama.presets = {
          programs = {
            carapace.enable = mkDefault true;
            dictation.enable = mkDefault true;
            editor.enable = mkDefault true;
            gh.enable = mkDefault true;
            spotify.enable = mkDefault true;
          };

          services = {
            ssh-agent.enable = mkDefault true;
          };
        };

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
