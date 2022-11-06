{ config, lib, ... }:

let cfg = config.presets.shell-environment;

in with lib; {
  imports = [ ./bat.nix ./exa.nix ];

  options.presets.shell-environment.enable =
    mkEnableOption "Use an opinionated shell environment";

  config.presets = mkIf cfg.enable {
    exa.enable = mkDefault true;
    bat.enable = mkDefault true;
  };
}
