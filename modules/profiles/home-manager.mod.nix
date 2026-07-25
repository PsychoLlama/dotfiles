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
    "${self.presets.programs.dive}".enable = mkDefault true;
    "${self.presets.programs.dix}".enable = mkDefault true;
    "${self.presets.programs.doggo}".enable = mkDefault true;
    "${self.presets.programs.editor}".enable = mkDefault true;
    "${self.presets.programs.fd}".enable = mkDefault true;
    "${self.presets.programs.fzf}".enable = mkDefault true;
    "${self.presets.programs.gh}".enable = mkDefault true;
    "${self.presets.programs.git}".enable = mkDefault true;
    "${self.presets.programs.glow}".enable = mkDefault true;
    "${self.presets.programs.jq}".enable = mkDefault true;
    "${self.presets.programs.miniserve}".enable = mkDefault true;
    "${self.presets.programs.nix-output-monitor}".enable = mkDefault true;
    "${self.presets.programs.nushell}".enable = mkDefault true;
    "${self.presets.programs.python3}".enable = mkDefault true;
    "${self.presets.programs.spotify}".enable = mkDefault true;
    "${self.presets.programs.starship}".enable = mkDefault true;
    "${self.presets.programs.termshark}".enable = mkDefault true;
    "${self.presets.programs.tmux}".enable = mkDefault true;
    "${self.presets.programs.viddy}".enable = mkDefault true;
    "${self.presets.programs.wezterm}".enable = mkDefault true;
    "${self.presets.programs.whois}".enable = mkDefault true;
    "${self.presets.programs.zoxide}".enable = mkDefault true;

    "${self.presets.services.ssh-agent}".enable = mkDefault true;
  };

  # Platform extensions that never had a preset. A platform block reaches the
  # host's own options directly.
  platforms.home-manager.programs = {
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
}
