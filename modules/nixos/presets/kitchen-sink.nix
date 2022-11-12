{ config, lib, ... }:

let cfg = config.dotfiles.presets.kitchen-sink;

in with lib; {
  options.dotfiles.presets.kitchen-sink.enable =
    mkEnableOption "Enable everything by default";

  config.dotfiles.presets = mkIf cfg.enable {
    god-mode.enable = mkDefault true;
    greetd.enable = mkDefault true;
    network-management.enable = mkDefault true;
  };
}
