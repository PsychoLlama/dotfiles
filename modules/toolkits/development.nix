{ config, unstable, lib, ... }:

let
  df = config.dotfiles;
  cfg = df.toolkit.development;

in {
  options.dotfiles.toolkit.development = with lib; {
    enable = mkOption {
      type = types.bool;
      description = "Enable the development toolkit";
      default = df.kitchen-sink.enable;
    };

    aliases.enable = mkOption {
      type = types.bool;
      description = "Enable short git aliases";
      default = df.kitchen-sink.enable;
    };
  };

  config = with lib; {
    environment.etc.gitconfig.source = mkIf cfg.enable ../../config/git.ini;

    environment.systemPackages = with unstable;
      mkIf cfg.enable [ git gitAndTools.delta miniserve nixfmt shellcheck ];

    environment.shellAliases = mkIf (cfg.enable && cfg.aliases.enable) {
      g = "git";
      c = "git commit";
      b = "git branch";
      ch = "git checkout";
      h = "git diff HEAD";
      hh = "git diff HEAD~1";
      hhh = "git diff HEAD~2";
    };
  };
}
