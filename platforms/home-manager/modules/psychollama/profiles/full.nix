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
  options.psychollama.profiles.full = {
    enable = lib.mkEnableOption "Enable all dotfiles programs and services";
  };

  config = lib.mkIf cfg.enable {
    psychollama.presets = {
      programs = {
        bat.enable = mkDefault true;
        bottom.enable = mkDefault true;
        claude-code.enable = mkDefault true;
        codex.enable = mkDefault true;
        dictation.enable = mkDefault true;
        direnv.enable = mkDefault true;
        dive.enable = mkDefault true;
        doggo.enable = mkDefault true;
        editor.enable = mkDefault true;
        emacs.enable = mkDefault true;
        fd.enable = mkDefault true;
        fzf.enable = mkDefault true;
        gh.enable = mkDefault true;
        git.enable = mkDefault true;
        glow.enable = mkDefault true;
        jq.enable = mkDefault true;
        miniserve.enable = mkDefault true;
        nix-output-monitor.enable = mkDefault true;
        nushell.enable = mkDefault true;
        spotify-player.enable = mkDefault true;
        starship.enable = mkDefault true;
        termshark.enable = mkDefault true;
        tmux.enable = mkDefault true;
        viddy.enable = mkDefault true;
        wezterm.enable = mkDefault true;
        whois.enable = mkDefault true;
        zellij.enable = mkDefault true;
        zoxide.enable = mkDefault true;
      };

      services = {
        emacs.enable = mkDefault true;
        auth-agent.enable = mkDefault true;
      };
    };

    programs = {
      binutils.enable = mkDefault true;
      duf.enable = mkDefault true;
      hexyl.enable = mkDefault true;
      onefetch.enable = mkDefault true;
      parted.enable = mkDefault pkgs.stdenv.isLinux;
      radare2.enable = mkDefault true;
      rage.enable = mkDefault true;
      ripgrep.enable = mkDefault true;
      tokei.enable = mkDefault true;
      viu.enable = mkDefault true;
    };
  };
}
