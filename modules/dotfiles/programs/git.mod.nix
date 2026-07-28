{
  self,
  cfg,
  lib,
  pkgs,
  ...
}:

let
  inherit (self) identity;
  inherit (cfg) fsmonitor;
in

{
  options.fsmonitor = {
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to back git's fsmonitor with watchman.";
    };

    package = lib.mkPackageOption pkgs.unstable "watchman" { };
  };

  config.programs.nushell.abbreviations = {
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

  modules.home-manager =
    { config, ... }:

    let
      # Git ships a reference watchman hook for fsmonitor protocol v2 (perl, v2
      # tokens). Upstream git has no Linux fsmonitor daemon, so we reuse the
      # hook and delegate watching to watchman (inotify/fanotify under the
      # hood).
      sampleHook = "${config.programs.git.package}/share/git-core/templates/hooks/fsmonitor-watchman.sample";

      watchScript = pkgs.writeShellApplication {
        name = "git-fsmonitor-watch";
        runtimeInputs = [
          config.programs.git.package
          fsmonitor.package
        ];
        text = ''
          git config core.fsmonitor ${sampleHook}
          git update-index --fsmonitor
        '';
      };

      unwatchScript = pkgs.writeShellApplication {
        name = "git-fsmonitor-unwatch";
        runtimeInputs = [ config.programs.git.package ];
        text = ''
          git config --unset core.fsmonitor
          git update-index --no-fsmonitor
        '';
      };
    in

    {
      home.packages = lib.optional fsmonitor.enable fsmonitor.package;

      programs.git = {
        enable = lib.mkDefault true;
        package = lib.mkDefault pkgs.unstable.git;

        settings = {
          user = {
            name = lib.mkDefault identity.name;
            email = lib.mkDefault identity.email;
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

            watch = lib.mkIf fsmonitor.enable "!${lib.getExe watchScript}";
            unwatch = lib.mkIf fsmonitor.enable "!${lib.getExe unwatchScript}";
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
    };
}
