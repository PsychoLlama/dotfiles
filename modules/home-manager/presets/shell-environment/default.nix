{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.shell-environment;

in {
  imports = [
    ./bat.nix
    ./bottom.nix
    ./direnv.nix
    ./dive.nix
    ./eza.nix
    ./fd.nix
    ./fzf.nix
    ./git.nix
    ./glow.nix
    ./jq.nix
    ./miniserve.nix
    ./ncspot.nix
    ./neovim.nix
    ./sshfs.nix
    ./w3m.nix
  ];

  options.presets.shell-environment.enable =
    mkEnableOption "Use an opinionated shell environment";

  config = mkIf cfg.enable {
    presets = {
      toolkits.networking.enable = mkDefault true;

      bat.enable = mkDefault true;
      bottom.enable = mkDefault true;
      direnv.enable = mkDefault true;
      dive.enable = mkDefault true;
      eza.enable = mkDefault true;
      fd.enable = mkDefault true;
      fzf.enable = mkDefault true;
      git.enable = mkDefault true;
      glow.enable = mkDefault true;
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
      hexyl.enable = mkDefault true;
      ipfs.enable = mkDefault true;
      litecli.enable = mkDefault true;
      lnav.enable = mkDefault true;
      onefetch.enable = mkDefault true;
      parted.enable = mkDefault pkgs.stdenv.isLinux;
      pv.enable = mkDefault true;
      rage.enable = mkDefault true;
      ripgrep.enable = mkDefault true;
      tokei.enable = mkDefault true;
      viu.enable = mkDefault true;
    };
  };
}
