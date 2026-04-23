{
  pkgs,
  lib,
  config,
  ...
}:

let
  cfg = config.psychollama.presets.programs.claude-code;
in

{
  options.psychollama.presets.programs.claude-code = {
    enable = lib.mkEnableOption "Opinionated config for Claude Code";
  };

  config = lib.mkIf cfg.enable {
    home.shellAliases.a = "claude"; # `a` short for `agent`

    programs.claude-code = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.unstable.claude-code-bin;

      memory.text = ''
        # Environment

        - Nix is installed with `nix-command flakes` enabled.
        - Prefer the `nix` command (`nix build` over `nix-build`, `nix shell` over `nix-shell`, etc).
        - Prefer `fd` over `find`.
        - Prefer `rg` over `grep`.

        # Commit Messages

        - Imperative title, descriptive body (markdown).
        - Capture context not otherwise available (goal, failed approaches, decisions, etc).

        # Capabilities

        - `${config.programs.claude-code.scripts.notify.path} <message>` sends a desktop notification. Use it to get my attention after completing a long-running task.

        # Authoring Agent Files

        - `AGENTS.md` is the source of truth. `CLAUDE.md` should only contain `@AGENTS.md`.
        - Author `AGENTS.md` with short, declarative statements.
      '';

      skills = {
        using-neovim = ./skills/using-neovim;
        using-nix = ./skills/using-nix;
      };

      commands.repo-update = ./commands/repo-update.md;

      settings = {
        theme = "dark";
        preferredNotifChannel = "terminal_bell";

        # settings.json is a read-only symlink into the Nix store, so Claude
        # Code's "don't ask again" toggle for auto mode can never persist.
        # Opt out of the confirmation up front.
        skipAutoPermissionPrompt = true;

        attribution = {
          commit = "";
          pr = "";
        };

        env = {
          # I don't want uncommitted memory affecting Claude's decisions.
          CLAUDE_CODE_DISABLE_AUTO_MEMORY = "1";
        };

        permissions = {
          defaultMode = "auto";

          additionalDirectories = [
            "${config.home.homeDirectory}/projects/psychollama"
            "${config.home.homeDirectory}/projects/@learn"
          ];

          allow = [
            # Mostly read-only, but dangerously allows reading outside the workdir.
            "Bash(cat:*)"
            "Bash(date:*)"
            "Bash(fd:*)"
            "Bash(file:*)"
            "Bash(find:*)"
            "Bash(grep:*)"
            "Bash(head:*)"
            "Bash(jq:*)"
            "Bash(ls:*)"
            "Bash(mkdir:*)"
            "Bash(nvim:*)"
            "Bash(pwd:*)"
            "Bash(rg:*)"
            "Bash(tail:*)"
            "Bash(tree:*)"
            "Bash(wc:*)"
            "Bash(which:*)"
            "Bash(xxd:*)"

            # Local git commands (no remote effects)
            "Bash(git add:*)"
            "Bash(git bisect:*)"
            "Bash(git blame:*)"
            "Bash(git branch:*)"
            "Bash(git checkout:*)"
            "Bash(git cherry-pick:*)"
            "Bash(git clean:*)"
            "Bash(git commit:*)"
            "Bash(git diff:*)"
            "Bash(git fetch:*)"
            "Bash(git grep:*)"
            "Bash(git log:*)"
            "Bash(git ls-remote:*)"
            "Bash(git merge:*)"
            "Bash(git rebase:*)"
            "Bash(git remote show:*)"
            "Bash(git reset:*)"
            "Bash(git restore:*)"
            "Bash(git revert:*)"
            "Bash(git rm:*)"
            "Bash(git show:*)"
            "Bash(git stash:*)"
            "Bash(git status:*)"
            "Bash(git tag:*)"
            "Bash(git worktree:*)"

            "Bash(gh attestation verify:*)"
            "Bash(gh cache list:*)"
            "Bash(gh gist list:*)"
            "Bash(gh gist view:*)"
            "Bash(gh issue list:*)"
            "Bash(gh issue status:*)"
            "Bash(gh issue view:*)"
            "Bash(gh label list:*)"
            "Bash(gh pr checkout:*)"
            "Bash(gh pr checks:*)"
            "Bash(gh pr diff:*)"
            "Bash(gh pr list:*)"
            "Bash(gh pr status:*)"
            "Bash(gh pr view:*)"
            "Bash(gh project field-list:*)"
            "Bash(gh project item-list:*)"
            "Bash(gh project list:*)"
            "Bash(gh project view:*)"
            "Bash(gh release list:*)"
            "Bash(gh release view:*)"
            "Bash(gh repo list:*)"
            "Bash(gh repo view:*)"
            "Bash(gh ruleset check:*)"
            "Bash(gh ruleset list:*)"
            "Bash(gh ruleset view:*)"
            "Bash(gh run download:*)"
            "Bash(gh run list:*)"
            "Bash(gh run view:*)"
            "Bash(gh run watch:*)"
            "Bash(gh search code:*)"
            "Bash(gh search commits:*)"
            "Bash(gh search issues:*)"
            "Bash(gh search prs:*)"
            "Bash(gh search repos:*)"
            "Bash(gh secret list:*)"
            "Bash(gh status:*)"
            "Bash(gh variable get:*)"
            "Bash(gh variable list:*)"
            "Bash(gh workflow list:*)"
            "Bash(gh workflow view:*)"

            # Generic Project Tools
            "Bash(just:*)"
            "Bash(prek:*)"
            "Bash(prettier:*)"
            "Bash(treefmt:*)"

            # Rust Projects
            "Bash(cargo --list:*)"
            "Bash(cargo build:*)"
            "Bash(cargo check:*)"
            "Bash(cargo clippy:*)"
            "Bash(cargo doc:*)"
            "Bash(cargo fmt:*)"
            "Bash(cargo run:*)"
            "Bash(cargo search:*)"
            "Bash(cargo test:*)"
            "Bash(rustc:*)"

            # JavaScript / TypeScript projects
            "Bash(npm info:*)"
            "Bash(npm run)"
            "Bash(npm run:*)"
            "Bash(npm t:*)"
            "Bash(npm test:*)"
            "Bash(npm view:*)"
            "Bash(pnpm info:*)"
            "Bash(pnpm run)"
            "Bash(pnpm run:*)"
            "Bash(pnpm t:*)"
            "Bash(pnpm test:*)"
            "Bash(pnpm view:*)"

            # Nix projects
            "Bash(dix:*)"
            "Bash(nix build:*)"
            "Bash(nix derivation show:*)"
            "Bash(nix develop:*)"
            "Bash(nix eval:*)"
            "Bash(nix flake check:*)"
            "Bash(nix flake lock:*)"
            "Bash(nix flake metadata:*)"
            "Bash(nix flake show:*)"
            "Bash(nix flake update:*)"
            "Bash(nix fmt:*)"
            "Bash(nix hash:*)"
            "Bash(nix log:*)"
            "Bash(nix path-info:*)"
            "Bash(nix repl:*)"
            "Bash(nix run:*)"
            "Bash(nix search:*)"
            "Bash(nix why-depends:*)"
            "Read(//nix/store/**)" # Read-only source diving

            # Lua projects
            "Bash(luacheck:*)"

            # System Management
            "Bash(journalctl:*)"
            "Bash(systemctl list-units:*)"
            "Bash(systemctl status:*)"

            "WebFetch(domain:docs.anthropic.com)"
            "WebFetch(domain:docs.claude.com)"
            "WebFetch(domain:platform.openai.com)"
          ];
        };
      };
    };

    programs.git = lib.mkIf cfg.enable {
      ignores = [
        "**/.claude/*.lock"
        "**/.claude/settings.local.json"
        "**/.claude/worktrees"
        "**/CLAUDE.local.md"
      ];
    };
  };
}
