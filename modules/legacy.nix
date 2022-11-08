{ config, lib, ... }:

# TODO: Kill or rewrite everything in this file to be less horrible.

let cfg = config.dotfiles;

in with lib; {
  options.dotfiles = {
    passwordless-sudo.enable = mkEnableOption "Enable passwordless sudo";
    wireless.enable = mkEnableOption "Enable wireless networking utilities";
  };

  config = mkMerge [
    {
      security.sudo.wheelNeedsPassword = !cfg.passwordless-sudo.enable;
      programs.wireshark.enable =
        lib.mkDefault config.dotfiles.kitchen-sink.enable;
    }

    (mkIf cfg.wireless.enable {
      networking.networkmanager.enable = true;
      users.users.${cfg.user.account}.extraGroups = [ "networkmanager" ];
    })
  ];
}
