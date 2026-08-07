{
  imports = [
    ../programs/claude-code
    ../programs/direnv.nix
    ../programs/sway.nix
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
            bat.enable = mkDefault true;
            bottom.enable = mkDefault true;
            carapace.enable = mkDefault true;
            delta.enable = mkDefault true;
            dictation.enable = mkDefault true;
            dix.enable = mkDefault true;
            dive.enable = mkDefault true;
            doggo.enable = mkDefault true;
            editor.enable = mkDefault true;
            fd.enable = mkDefault true;
            fzf.enable = mkDefault true;
            gh.enable = mkDefault true;
            git.enable = mkDefault true;
            glow.enable = mkDefault true;
            jq.enable = mkDefault true;
            miniserve.enable = mkDefault true;
            nix-output-monitor.enable = mkDefault true;
            nushell.enable = mkDefault true;
            python3.enable = mkDefault true;
            spotify.enable = mkDefault true;
            starship.enable = mkDefault true;
            termshark.enable = mkDefault true;
            tmux.enable = mkDefault true;
            viddy.enable = mkDefault true;
            wezterm.enable = mkDefault true;
            whois.enable = mkDefault true;
            zoxide.enable = mkDefault true;
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
