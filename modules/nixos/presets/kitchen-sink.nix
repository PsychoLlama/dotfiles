{ config, lib, ... }:

with lib;

let cfg = config.dotfiles.presets.kitchen-sink;

in {
  options.dotfiles.presets.kitchen-sink.enable =
    mkEnableOption "Enable everything by default";

  config.dotfiles.presets = mkIf cfg.enable {
    desktop-environment.enable = mkDefault true;
    god-mode.enable = mkDefault true;
    greetd.enable = mkDefault true;
    network-management.enable = mkDefault true;
  };
}
