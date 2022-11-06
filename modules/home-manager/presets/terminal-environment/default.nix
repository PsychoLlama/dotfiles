{ config, lib, ... }:

let cfg = config.presets.terminal-environment;

in with lib; {
  imports = [ ./alacritty.nix ];

  options.presets.terminal-environment.enable =
    mkEnableOption "Use an opinionated terminal environment";

  config.presets = mkIf cfg.enable { alacritty.enable = mkDefault true; };
}
