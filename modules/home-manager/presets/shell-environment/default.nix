{ config, lib, pkgs, ... }:

let cfg = config.presets.shell-environment;

in with lib; {
  imports = [
    ./bat.nix
    ./bottom.nix
    ./exa.nix
    ./fd.nix
    ./fzf.nix
    ./git.nix
    ./jq.nix
    ./miniserve.nix
    ./ncspot.nix
    ./neovim.nix
    ./sshfs.nix
    ./dive.nix
    ./w3m.nix
  ];

  options.presets.shell-environment.enable =
    mkEnableOption "Use an opinionated shell environment";

  config = mkIf cfg.enable {
    presets = {
      toolkits.networking.enable = mkDefault true;

      bat.enable = mkDefault true;
      bottom.enable = mkDefault true;
      dive.enable = mkDefault true;
      exa.enable = mkDefault true;
      fd.enable = mkDefault true;
      fzf.enable = mkDefault true;
      git.enable = mkDefault true;
      jq.enable = mkDefault true;
      miniserve.enable = mkDefault true;
      ncspot.enable = mkDefault true;
      neovim.enable = mkDefault true;
      sshfs.enable = mkDefault pkgs.stdenv.isLinux;
      w3m.enable = mkDefault true;
    };

    programs = {
      binutils.enable = mkDefault true;
      du-dust.enable = mkDefault true;
      duf.enable = mkDefault true;
      glow.enable = mkDefault true;
      hexyl.enable = mkDefault true;
      ipfs.enable = mkDefault true;
      litecli.enable = mkDefault true;
      lnav.enable = mkDefault true;
      parted.enable = mkDefault pkgs.stdenv.isLinux;
      pv.enable = mkDefault true;
      rage.enable = mkDefault true;
      ripgrep.enable = mkDefault true;
      tokei.enable = mkDefault true;
      viu.enable = mkDefault true;
    };
  };
}
