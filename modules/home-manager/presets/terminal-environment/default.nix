{ config, lib, ... }:

let cfg = config.presets.terminal-environment;

in with lib; {
  imports = [ ./alacritty.nix ./starship.nix ./zoxide.nix ];

  options.presets.terminal-environment.enable =
    mkEnableOption "Use an opinionated terminal environment";

  config.presets = mkIf cfg.enable {
    alacritty.enable = mkDefault true;
    starship.enable = mkDefault true;
    zoxide.enable = mkDefault true;
  };
}
