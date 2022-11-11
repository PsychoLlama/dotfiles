{ config, lib, ... }:

# TODO: Kill or rewrite everything in this file to be less horrible.

let cfg = config.dotfiles;

in with lib; {
  options.dotfiles.wireless.enable =
    mkEnableOption "Enable wireless networking utilities";

  config = mkIf cfg.wireless.enable {
    networking.networkmanager.enable = true;
    users.users.${cfg.user.name}.extraGroups = [ "networkmanager" ];
  };
}
