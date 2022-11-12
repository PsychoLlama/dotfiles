{ config, lib, pkgs, ... }:

let cfg = config.dotfiles.presets.desktop-environment;

in with lib; {
  options.dotfiles.presets.desktop-environment.enable =
    mkEnableOption "Configure a desktop environment";

  config = mkIf cfg.enable {
    hardware.pulseaudio.enable = mkDefault true;
    services.printing.enable = mkDefault true;
    sound.enable = mkDefault true;
  };
}
