{ config, lib, ... }:

with lib;

let
  cfg = config.presets.programs.git;
in
{
  options.presets.programs.git.enable = mkEnableOption "Provide an opinionated configuration for git";

  config = mkIf cfg.enable {
    home.shellAliases = {
      g = "git";
      c = "git commit";
      b = "git branch";
      ch = "git checkout";
      h = "git diff HEAD --staged";
      hh = "git diff HEAD~1";
      hhh = "git diff HEAD~2";
    };

    programs.git = {
      enable = true;

      aliases = {
        a = "add --all";
        d = "diff";
        b = "branch";
        l = "log -1000 --format='%Cgreen%h%Creset: %an (%C(yellow)%ar%Creset)%n%s%n%n%b'";
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

      delta = {
        enable = true;

        options = {
          dark = true;
          syntax-theme = "OneHalfDark";
        };
      };

      extraConfig = {
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

        core = {
          editor = "nvim";
          untrackedCache = true;
          fsmonitor = true;
        };
      };
    };
  };
}
