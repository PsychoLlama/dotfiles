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

  config = with lib;
    mkIf cfg.enable {
      users.groups.${cfg.group.name}.members = [ df.user.account ];

      security.sudo.extraRules = [{
        groups = [ cfg.group.name ];
        commands = [{
          command = "ALL";
          options = [ "NOPASSWD" ];
        }];
      }];
    };
}
