{ units, define, ... }:

let
  inherit (units.platform) home-manager user;
  inherit (units.programs) nushell;
in

{
  unit = define.unit {
    uses = {
      inherit (user) identity;
    };

    config = { identity }: [
      (home-manager.lib.program "git")

      (nushell.abbreviation {
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
      })

      (home-manager.module (
        {
          config,
          lib,
          pkgs,
          ...
        }:

        let
          inherit (identity) name email;

          git = config.programs.git.package;

          # Upstream git has no Linux fsmonitor daemon, so watching is
          # delegated to watchman (inotify/fanotify under the hood). Same
          # version as unstable, which currently fails to build (folly vs gcc
          # 15.3).
          watchman = pkgs.watchman;

          # Git ships a reference watchman hook for fsmonitor protocol v2
          # (perl, v2 tokens).
          hook = "${git}/share/git-core/templates/hooks/fsmonitor-watchman.sample";

          # Opts the current repo into watchman fsmonitor.
          watch = pkgs.writeShellApplication {
            name = "git-fsmonitor-watch";
            runtimeInputs = [
              git
              watchman
            ];
            text = ''
              git config core.fsmonitor ${hook}
              git update-index --fsmonitor
            '';
          };

          # Opts the current repo out of it.
          unwatch = pkgs.writeShellApplication {
            name = "git-fsmonitor-unwatch";
            runtimeInputs = [ git ];
            text = ''
              git config --unset core.fsmonitor
              git update-index --no-fsmonitor
            '';
          };
        in

        {
          home.packages = [ watchman ];

          programs.git.ignores = [ ".ignore/" ];

          programs.git.settings = {
            user = {
              name = lib.mkDefault name;
              email = lib.mkDefault email;
            };

            alias = {
              c = "commit";
              review = ''!git show "$(git merge-base --fork-point origin/HEAD)"..HEAD'';
              f = "fetch origin";
              pf = "push --force-with-lease";
              s = "stash";
              ss = "stash push --staged --message";
              pl = "pull origin";
              amend = "commit --amend";

              watch = "!${lib.getExe watch}";
              unwatch = "!${lib.getExe unwatch}";
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

            advice.forceDeleteBranch = false;
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
        }
      ))
    ];
  };
}
