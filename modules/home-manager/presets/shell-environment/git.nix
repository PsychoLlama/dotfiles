{ config, lib, ... }:

let cfg = config.presets.git;

in with lib; {
  options.presets.git.enable = mkEnableOption "Replace ls with git";

  config = mkIf cfg.enable {
    home.shellAliases.g = "git";

    programs.git = {
      enable = true;

      aliases = {
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

        # TODO: Move to the editor module, when it exists.
        core.editor = "nvim";
      };
    };
  };
}
