{ config, unstable, lib, ... }:

let
  df = config.dotfiles;
  cfg = df.toolkit.infrastructure;

in {
  options.dotfiles.toolkit.infrastructure = with lib; {
    enable = mkOption {
      type = types.bool;
      description = "Enable the infrastructure development toolkit";
      default = df.kitchen-sink.enable;
    };

    docker.package = mkOption {
      type = types.package;
      description = "Which docker package to use";
      default = unstable.docker;
    };
  };

  config = with lib; {
    environment.systemPackages = with unstable;
      mkIf cfg.enable [ ipmitool kubectl terraform_1_0_0 ];

    virtualisation.docker = mkIf cfg.enable {
      enable = mkDefault true;
      package = cfg.docker.package;
      autoPrune.enable = true;
    };

    users.users.${df.user.account}.extraGroups = mkIf cfg.enable [ "docker" ];
  };
}
