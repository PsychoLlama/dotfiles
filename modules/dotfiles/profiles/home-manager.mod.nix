{
  lib,
  pkgs,
  ...
}:

# Every dotfiles program and service that isn't tied to a desktop. Was the
# home-manager `full` profile.

let
  inherit (lib) mkDefault;
in

{
  # `config` is this plugin's own namespace; the mount point is implied. A typo
  # here is an unknown-option error from the merge machinery, not a
  # silently-ignored option path.
  config = {
    programs = {
      bat.enable = mkDefault true;
      bottom.enable = mkDefault true;
      carapace.enable = mkDefault true;
      claude-code.enable = mkDefault true;
      delta.enable = mkDefault true;
      dictation.enable = mkDefault true;
      direnv.enable = mkDefault true;
      dive.enable = mkDefault true;
      dix.enable = mkDefault true;
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
      binutils.enable = mkDefault true;
      duf.enable = mkDefault true;
      hexyl.enable = mkDefault true;
      lsof.enable = mkDefault true;
      onefetch.enable = mkDefault true;
      parted.enable = mkDefault pkgs.stdenv.isLinux;
      rage.enable = mkDefault true;
      tokei.enable = mkDefault true;
      viddy.enable = mkDefault true;
      viu.enable = mkDefault true;
      wezterm.enable = mkDefault true;
      whois.enable = mkDefault true;
      zoxide.enable = mkDefault true;
    };

    services.ssh-agent.enable = mkDefault true;
  };

  # Programs home-manager already models well enough to need no preset. A
  # class block reaches that host's own options directly.
  modules.home-manager.programs = {
    man.generateCaches = mkDefault true;
    nh.enable = mkDefault true;
    ripgrep.enable = mkDefault true;
  };
}
