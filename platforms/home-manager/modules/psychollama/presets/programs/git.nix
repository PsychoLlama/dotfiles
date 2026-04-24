{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.git;
  fsmonitor = config.programs.git.fsmonitor;
in

{
  config = lib.mkIf cfg.enable {
    programs.git.fsmonitor = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.unstable.watchman;
    };

    home.shellAliases = {
      g = "git";
      b = "git branch";
      ch = "git checkout";
      h = "git diff HEAD --staged";
      hh = "git diff HEAD~1";
      hhh = "git diff HEAD~2";
    };

    programs.git.settings = {
      alias = {
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
        ss = "stash push --staged --message";
        pl = "pull origin";
        amend = "commit --amend";

        watch = lib.mkIf fsmonitor.enable "!${lib.getExe fsmonitor.watchScript}";
        unwatch = lib.mkIf fsmonitor.enable "!${lib.getExe fsmonitor.unwatchScript}";
      };

      push = {
        autoSetupRemote = true;
        default = "current";
        followTags = true;
      };

      fetch = {
        prune = true;
        pruneTags = true;
        writeCommitGraph = true;
        negotiationAlgorithm = "skipping";
      };

      pull = {
        rebase = true;
        ff = "only";
      };

      init.defaultBranch = "main";
      rebase.autoStash = true;
      interactive.singleKey = true;

      feature.manyFiles = true;

      core = {
        editor = "nvim";
        untrackedCache = true;
      };

      index.skipHash = true;
      pack.useBitmapBoundaryTraversal = true;
      transfer.fsckObjects = true;

      diff = {
        algorithm = "histogram";
        colorMoved = "default";
        colorMovedWS = "allow-indentation-change";
      };

      merge.conflictStyle = "zdiff3";

      rerere = {
        enabled = true;
        autoUpdate = true;
      };

      branch.sort = "-committerdate";
      tag.sort = "version:refname";
      column.ui = "auto";
      log.date = "iso";

      submodule = {
        recurse = true;
        fetchJobs = 0;
      };
    };

    programs.delta = {
      enable = true;
      enableGitIntegration = true;

      options = {
        dark = true;
        syntax-theme = "OneHalfDark";
      };
    };
  };
}
