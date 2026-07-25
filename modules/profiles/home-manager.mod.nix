{
  self,
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
  # Writes addressed by handle. A typo here is an eval error at the
  # reference, not a silently-ignored option path.
  config = {
    "${self.presets.programs.claude-code}".enable = mkDefault true;
    "${self.presets.programs.dictation}".enable = mkDefault true;
    "${self.presets.programs.editor}".enable = mkDefault true;
    "${self.presets.programs.gh}".enable = mkDefault true;
    "${self.presets.programs.git}".enable = mkDefault true;
  };

  # Presets that still live in the old `psychollama.presets.*` namespace, plus
  # the platform extensions that never had a preset. A platform block reaches
  # the host's own options directly, so the two systems coexist for as long as
  # the migration takes.
  platforms.home-manager = {
    psychollama.presets = {
      programs = {
        bat.enable = mkDefault true;
        bottom.enable = mkDefault true;
        carapace.enable = mkDefault true;
        delta.enable = mkDefault true;
        direnv.enable = mkDefault true;
        dix.enable = mkDefault true;
        dive.enable = mkDefault true;
        doggo.enable = mkDefault true;
        fd.enable = mkDefault true;
        fzf.enable = mkDefault true;
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
