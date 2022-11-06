{ config, lib, ... }:

let cfg = config.presets.shell-environment;

in with lib; {
  imports = [ ./bat.nix ./exa.nix ./git.nix ./neovim.nix ];

  options.presets.shell-environment.enable =
    mkEnableOption "Use an opinionated shell environment";

  config.presets = mkIf cfg.enable {
    bat.enable = mkDefault true;
    exa.enable = mkDefault true;
    git.enable = mkDefault true;
    neovim.enable = mkDefault true;
  };
}
