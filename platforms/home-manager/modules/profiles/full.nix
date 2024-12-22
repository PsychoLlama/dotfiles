{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.profiles.full;
in
{
  options.profiles.full.enable = mkEnableOption "Enable all dotfiles programs and services";

  config = mkIf cfg.enable {
    presets = {
      programs = {
        aider-chat.enable = mkDefault true;
        ast-grep.enable = mkDefault true;
        bat.enable = mkDefault true;
        bottom.enable = mkDefault true;
        direnv.enable = mkDefault true;
        dive.enable = mkDefault true;
        doggo.enable = mkDefault true;
        editor.enable = mkDefault true;
        fd.enable = mkDefault true;
        fzf.enable = mkDefault true;
        git.enable = mkDefault true;
        glow.enable = mkDefault true;
        jq.enable = mkDefault true;
        miniserve.enable = mkDefault true;
        nmap.enable = mkDefault true;
        nushell.enable = mkDefault true;
        spotify-player.enable = mkDefault true;
        sshfs.enable = mkDefault pkgs.stdenv.isLinux;
        starship.enable = mkDefault true;
        termshark.enable = mkDefault true;
        tmux.enable = mkDefault true;
        viddy.enable = mkDefault true;
        wezterm.enable = mkDefault true;
        whois.enable = mkDefault true;
        zoxide.enable = mkDefault true;
      };

      services = {
        auth-agent.enable = mkDefault true;
      };
    };

    programs = {
      binutils.enable = mkDefault true;
      du-dust.enable = mkDefault true;
      duf.enable = mkDefault true;
      hexyl.enable = mkDefault true;
      ipfs.enable = mkDefault pkgs.stdenv.isLinux;
      mods.enable = mkDefault true;
      onefetch.enable = mkDefault true;
      parted.enable = mkDefault pkgs.stdenv.isLinux;
      pv.enable = mkDefault true;
      radare2.enable = mkDefault true;
      rage.enable = mkDefault true;
      ripgrep.enable = mkDefault true;
      tokei.enable = mkDefault true;
      viu.enable = mkDefault true;
    };
  };
}
