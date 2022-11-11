{ config, lib, pkgs, ... }:

let cfg = config.dotfiles.presets.network-management;

in with lib; {
  options.dotfiles.presets.network-management.enable =
    mkEnableOption "Configure network management tools";

  config = mkIf cfg.enable {
    networking.networkmanager.enable = true;
    dotfiles.user.extraGroups = [ "networkmanager" ];
  };
}
