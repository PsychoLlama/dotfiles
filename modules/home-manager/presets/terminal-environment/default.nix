{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.terminal-environment;

in {
  imports = [
    ./alacritty.nix
    ./nushell.nix
    ./ssh-agent.nix
    ./starship.nix
    ./tmux.nix
    ./zoxide.nix
    ./zsh.nix
  ];

  options.presets.terminal-environment.enable =
    mkEnableOption "Use an opinionated terminal environment";

  config.presets = mkIf cfg.enable {
    alacritty.enable = mkDefault true;
    nushell.enable = mkDefault true;
    shell-environment.enable = mkDefault true;
    ssh-agent.enable = mkDefault true;
    starship.enable = mkDefault true;
    tmux.enable = mkDefault true;
    zoxide.enable = mkDefault true;
    zsh.enable = mkDefault true;
  };
}
