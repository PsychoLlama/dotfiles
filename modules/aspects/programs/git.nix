{ config, ... }:

# Bound out here because the module below shadows `config` with its own.
let
  inherit (config.identity) name email;
in

{
  imports = [
    (import ./_mk-unstable-preset.nix "git")

    ../../platform/homeManager/programs/git.nix
    ../../platform/homeManager/programs/nushell/abbreviations.nix
    ../../rhizome/identity.nix
  ];

  flake.modules.homeManager.default =
    {
      config,
      lib,
      pkgs,
      ...
    }:

    let
      fsmonitor = config.programs.git.fsmonitor;
    in

    {
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
        gl = "git log";
        gp = "git push";
        ga = "git add --intent-to-add .";
        gaa = "git add --all";
        gr = "git reset";
        grr = "git reset --hard HEAD";
        grrr = "git reset --hard HEAD~1";
      };

      programs.git.settings = {
        user = {
          name = lib.mkDefault name;
          email = lib.mkDefault email;
        };

        alias = {
          c = "commit";
          review = ''!git diff "$(git merge-base --fork-point origin/HEAD)"'';
          f = "fetch origin";
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
