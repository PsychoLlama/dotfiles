{ config, unstable, lib, ... }:

let cfg = config.dotfiles.toolkit.development;

in {
  options.dotfiles.toolkit.development = with lib; {
    enable = mkOption {
      type = types.bool;
      description = "Enable the development toolkit";
      default = true;
    };
  };

  config = with lib; {
    environment.etc.gitconfig.source = mkIf cfg.enable ../../config/git.ini;

    environment.systemPackages = with unstable;
      mkIf cfg.enable [ git gitAndTools.delta miniserve nixfmt shellcheck ];
  };
}
