{ config, unstable, lib, ... }:

let
  df = config.dotfiles;
  cfg = df.wireless;

in {
  options.dotfiles.wireless = with lib; {
    enable = mkOption {
      type = types.bool;
      description = "Enable wireless networking utilities";
      default = df.kitchen-sink.enable;
    };
  };

  config = with lib; {
    networking.networkmanager.enable = cfg.enable;
    users.users.${df.user.account}.extraGroups =
      mkIf cfg.enable [ "networkmanager" ];
  };
}
