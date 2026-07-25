{ lib, pkgs, ... }:

# Every dotfiles program and service that isn't tied to a desktop. Was the
# home-manager `full` profile.

let
  inherit (lib) mkDefault;
in

{
  # Nothing here has migrated into the plugin yet, so the whole profile is
  # still a platform block.
  platforms.home-manager = {
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

      services.ssh-agent.enable = mkDefault true;
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
}
