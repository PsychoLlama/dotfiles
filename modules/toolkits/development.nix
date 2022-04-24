{ config, unstable, lib, pkgs, ... }:

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

  config = with lib;
    mkMerge [
      (mkIf cfg.enable {
        environment.etc.gitconfig.source = ../../config/git.ini;

        environment.systemPackages = with unstable; [
          pkgs.git # The newer version causes issues with Nix flakes.
          gitAndTools.delta
          miniserve
          nixfmt
          shellcheck
          sshfs
        ];
      })

      (mkIf (cfg.enable && cfg.aliases.enable) {
        environment.shellAliases = {
          g = "git";
          c = "git commit";
          b = "git branch";
          ch = "git checkout";
          h = "git diff HEAD";
          hh = "git diff HEAD~1";
          hhh = "git diff HEAD~2";
        };
      })
    ];
}
