{
  imports = [
    ../programs/bat.nix
    ../programs/bottom.nix
    ../programs/carapace.nix
    ../programs/claude-code/default.nix
    ../programs/codex/default.nix
    ../programs/delta.nix
    ../programs/dictation.nix
    ../programs/direnv.nix
    ../programs/dive.nix
    ../programs/dix.nix
    ../programs/doggo.nix
    ../programs/editor.nix
    ../programs/fd.nix
    ../programs/fzf.nix
    ../programs/gh.nix
    ../programs/git.nix
    ../programs/glow.nix
    ../programs/jq.nix
    ../programs/miniserve.nix
    ../programs/nix-output-monitor.nix
    ../programs/nushell/default.nix
    ../programs/nushell/swizzle.nix
    ../programs/python3.nix
    ../programs/spotify.nix
    ../programs/starship.nix
    ../programs/sway.nix
    ../programs/termshark.nix
    ../programs/tmux/default.nix
    ../programs/viddy.nix
    ../programs/wezterm.nix
    ../programs/whois.nix
    ../programs/wireshark.nix
    ../programs/zoxide.nix
    ../services/agenix.nix
    ../services/avahi.nix
    ../services/greetd.nix
    ../services/pipewire.nix
    ../services/podman.nix
    ../services/restic/default.nix
    ../services/ssh-agent.nix
    ../services/syncthing.nix
    ../services/tailscale.nix
    ../services/zfs.nix
    ../system/fonts.nix

    # Programs enabled directly below, without a preset of their own.
    ../../platform/homeManager/programs/binutils.nix
    ../../platform/homeManager/programs/duf.nix
    ../../platform/homeManager/programs/hexyl.nix
    ../../platform/homeManager/programs/lsof.nix
    ../../platform/homeManager/programs/onefetch.nix
    ../../platform/homeManager/programs/parted.nix
    ../../platform/homeManager/programs/rage.nix
    ../../platform/homeManager/programs/tokei.nix
    ../../platform/homeManager/programs/viu.nix
  ];

  exports = {
    nixos =
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

    homeManager =
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
