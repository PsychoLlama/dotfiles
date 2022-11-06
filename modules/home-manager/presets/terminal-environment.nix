{ config, lib, ... }:

let cfg = config.presets.terminal-environment;

in with lib; {
  options.presets.terminal-environment.enable =
    mkEnableOption "Use an opinionated terminal environment";

  config = mkIf cfg.enable { presets.alacritty.enable = mkDefault true; };
}
