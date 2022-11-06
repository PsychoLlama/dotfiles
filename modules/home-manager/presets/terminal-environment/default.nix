{ config, lib, ... }:

let cfg = config.presets.terminal-environment;

in with lib; {
  imports = [
    ./alacritty.nix
    ./exa.nix
    ./starship.nix
    ./tmux.nix
    ./zoxide.nix
    ./zsh.nix
  ];

  options.presets.terminal-environment.enable =
    mkEnableOption "Use an opinionated terminal environment";

  config.presets = mkIf cfg.enable {
    alacritty.enable = mkDefault true;
    exa.enable = mkDefault true;
    starship.enable = mkDefault true;
    tmux.enable = mkDefault true;
    zoxide.enable = mkDefault true;
    zsh.enable = mkDefault true;
  };
}
