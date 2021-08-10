{ config, lib, ... }:

let
  df = config.dotfiles;
  cfg = df.passwordless-sudo;

in {
  options.dotfiles.passwordless-sudo = with lib; {
    enable = mkOption {
      type = types.bool;
      description = "Enable passwordless sudo";
      default = df.kitchen-sink.enable;
    };

    group.name = mkOption {
      type = types.str;
      description = "The group name that has elevated permissions";
      default = "pantheon";
    };
  };

  config = with lib; {
    users.groups.${cfg.group.name} = mkIf cfg.enable { };
    users.users.${df.user.account}.extraGroups =
      mkIf cfg.enable [ cfg.group.name ];

    security.sudo.extraRules = mkIf cfg.enable [{
      groups = [ cfg.group.name ];
      commands = [{
        command = "ALL";
        options = [ "NOPASSWD" ];
      }];
    }];
  };
}
