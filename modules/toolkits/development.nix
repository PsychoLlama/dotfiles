{ config, unstable, lib, pkgs, ... }:

let
  df = config.dotfiles;
  cfg = df.toolkit.development;
  ini = pkgs.formats.ini { };

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

    git = {
      config = mkOption {
        type = ini.type;
        description = "Configuration to write to /etc/gitconfig";
        default = { };
      };

      package = mkOption {
        type = types.package;
        description = "Which package to install";
        default = unstable.git;
      };
    };
  };

  config = with lib;
    mkMerge [
      {
        dotfiles.toolkit.development.git.config = {
          alias = {
            a = "add --all";
            d = "diff";
            b = "branch";
            l =
              "log -1000 --format='%Cgreen%h%Creset: %an (%C(yellow)%ar%Creset)%n%s%n%n%b'";
            f = "fetch origin";
            r = "reset";
            rr = "reset --hard HEAD";
            rrr = "reset --hard HEAD^";
            p = "push --set-upstream origin";
            pf = "push --force-with-lease";
            s = "stash";
            ss = "stash save --include-untracked";
            pl = "pull origin";
            amend = "commit --amend";
          };

          user = {
            email = mkDefault "JesseTheGibson@gmail.com";
            name = "Jesse Gibson";
          };

          core = {
            editor = "nvim";
            pager =
              "${unstable.gitAndTools.delta}/bin/delta --dark --syntax-theme='OneHalfDark'";
          };

          push = {
            autoSetupRemote = true;
            default = "current";
          };

          fetch = {
            prune = true;
            pruneTags = true;
          };

          init.defaultBranch = "main";
          pull.rebase = true;
          rebase.autoStash = true;
          commit.verbose = true;
        };
      }

      (mkIf cfg.enable {
        environment.systemPackages = with unstable; [
          cfg.git.package
          gitAndTools.delta
          miniserve
          nixfmt
          shellcheck
          sshfs
        ];

        environment.etc.gitconfig.text = generators.toGitINI cfg.git.config;
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
