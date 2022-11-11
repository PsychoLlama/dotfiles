{ config, lib, ... }:

let cfg = config.dotfiles.presets.god-mode;

in with lib; {
  options.dotfiles.presets.god-mode.enable =
    mkEnableOption "Enable passwordless sudo";

  config = mkIf cfg.enable {
    dotfiles.user.extraGroups = [ "wheel" ];
    security.sudo.wheelNeedsPassword = false;
  };
}
