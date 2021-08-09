{ config, unstable, lib, ... }:

let cfg = config.dotfiles.toolkit.infrastructure;

in {
  options.dotfiles.toolkit.infrastructure = with lib; {
    enable = mkOption {
      type = types.bool;
      description = "Enable the infrastructure development toolkit";
      default = true;
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
  };
}
