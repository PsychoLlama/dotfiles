{ config, lib, ... }:

with lib;

let cfg = config.presets.terminal-environment;

in {
  imports = [
    ./alacritty.nix
    ./nushell.nix
    ./auth-agent.nix
    ./starship.nix
    ./tmux.nix
    ./zoxide.nix
    ./zsh.nix
  ];

  options.presets.terminal-environment.enable =
    mkEnableOption "Use an opinionated terminal environment";

  config = mkIf cfg.enable {
    programs.presets.enable = mkDefault true;

    presets = {
      alacritty.enable = mkDefault true;
      nushell.enable = mkDefault true;
      auth-agent.enable = mkDefault true;
      starship.enable = mkDefault true;
      tmux.enable = mkDefault true;
      zoxide.enable = mkDefault true;
      zsh.enable = mkDefault true;
    };
  };
}
