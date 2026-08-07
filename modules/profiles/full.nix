{ lib, ... }:

let
  option.options.psychollama.profiles.full = {
    enable = lib.mkEnableOption "Enable all dotfiles programs and services";
  };
in

{
  flake.modules = {
    nixos.default =
      { config, lib, ... }:

      let
        cfg = config.psychollama.profiles.full;
      in

      {
        imports = [ option ];

        config = lib.mkIf cfg.enable {
          psychollama.presets = {
            fonts.enable = lib.mkDefault true;

            services = {
              agenix.enable = lib.mkDefault true;
              avahi.enable = lib.mkDefault true;
              greetd.enable = lib.mkDefault true;
              pipewire.enable = lib.mkDefault true;
              podman.enable = lib.mkDefault true;
              restic.enable = lib.mkDefault true;
              syncthing.enable = lib.mkDefault true;
              tailscale.enable = lib.mkDefault true;
              zfs.enable = lib.mkDefault true;
            };

            programs = {
              codex.enable = lib.mkDefault true;
              sway.enable = lib.mkDefault true;
              wireshark.enable = lib.mkDefault true;
            };
          };

          services = {
            automatic-timezoned.enable = lib.mkDefault true;
            printing.enable = lib.mkDefault true;
          };

          # Build the apropos/whatis cache so `man -k` works. Carapace's native `man`
          # completer shells out to apropos, which returns nothing without it.
          documentation.man.cache.enable = lib.mkDefault true;
        };
      };

    homeManager.default =
      {
        config,
        lib,
        pkgs,
        ...
      }:

      let
        inherit (lib) mkDefault;
        cfg = config.psychollama.profiles.full;
      in

      {
        imports = [ option ];

        config = lib.mkIf cfg.enable {
          psychollama.presets = {
            programs = {
              bat.enable = mkDefault true;
              bottom.enable = mkDefault true;
              carapace.enable = mkDefault true;
              claude-code.enable = mkDefault true;
              delta.enable = mkDefault true;
              dictation.enable = mkDefault true;
              direnv.enable = mkDefault true;
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
  };
}
