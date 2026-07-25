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
    "${self.presets.programs.bat}".enable = mkDefault true;
    "${self.presets.programs.bottom}".enable = mkDefault true;
    "${self.presets.programs.carapace}".enable = mkDefault true;
    "${self.presets.programs.claude-code}".enable = mkDefault true;
    "${self.presets.programs.delta}".enable = mkDefault true;
    "${self.presets.programs.dictation}".enable = mkDefault true;
    "${self.presets.programs.direnv}".enable = mkDefault true;
    "${self.presets.programs.editor}".enable = mkDefault true;
    "${self.presets.programs.fd}".enable = mkDefault true;
    "${self.presets.programs.gh}".enable = mkDefault true;
    "${self.presets.programs.git}".enable = mkDefault true;
    "${self.presets.programs.glow}".enable = mkDefault true;
    "${self.presets.programs.nushell}".enable = mkDefault true;
    "${self.presets.programs.spotify}".enable = mkDefault true;
    "${self.presets.programs.starship}".enable = mkDefault true;
    "${self.presets.programs.tmux}".enable = mkDefault true;
    "${self.presets.programs.wezterm}".enable = mkDefault true;

    "${self.presets.services.ssh-agent}".enable = mkDefault true;
  };

  # Presets that still live in the old `psychollama.presets.*` namespace, plus
  # the platform extensions that never had a preset. A platform block reaches
  # the host's own options directly, so the two systems coexist for as long as
  # the migration takes.
  platforms.home-manager = {
    psychollama.presets.programs = {
      dix.enable = mkDefault true;
      dive.enable = mkDefault true;
      doggo.enable = mkDefault true;
      fzf.enable = mkDefault true;
      jq.enable = mkDefault true;
      miniserve.enable = mkDefault true;
      nix-output-monitor.enable = mkDefault true;
      python3.enable = mkDefault true;
      termshark.enable = mkDefault true;
      viddy.enable = mkDefault true;
      whois.enable = mkDefault true;
      zoxide.enable = mkDefault true;
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
