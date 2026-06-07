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

    programs.nushell.abbreviations = {
      g = "git";
      b = "git branch";
      ch = "git checkout";
      h = "git diff HEAD --staged";
      hh = "git diff HEAD~1";
      gd = "git diff";
      ga = "git add --all";
      gr = "git reset";
      grr = "git reset --hard HEAD";
      grrr = "git reset --hard HEAD~1";
    };

    programs.git.settings = {
      user = {
        name = lib.mkDefault config.psychollama.identity.name;
        email = lib.mkDefault config.psychollama.identity.email;
      };

      alias = {
        l = "log -1000 --format='%Cgreen%h%Creset: %an (%C(yellow)%ar%Creset)%n%s%n%n%b'";
        f = "fetch origin";
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

      diff.algorithm = "histogram";
      merge.conflictStyle = "zdiff3";

      rerere = {
        enabled = true;
        autoUpdate = true;
      };

      branch.sort = "-committerdate";
      tag.sort = "version:refname";
      log.date = "iso";

      submodule = {
        recurse = true;
        fetchJobs = 0;
      };
    };
  };
}
