{
  pkgs,
  lib,
  config,
  ...
}:

let
  cfg = config.psychollama.presets.programs.claude-code;
  coreEditorDoc = pkgs.writeText "neovim-framework.md" (builtins.readFile ./neovim-framework.md);
in

{
  options.psychollama.presets.programs.claude-code = {
    enable = lib.mkEnableOption "Opinionated config for Claude Code";
  };

  config = lib.mkIf cfg.enable {
    programs.claude-code = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.unstable.claude-code;

      commands = {
        "feature-freeze" = ./commands/feature-freeze.md;
      };

      memory.text = ''
        # Environment

        Nix is available. Use `nix develop` to run programs (not `nix shell` or `nix run`).

        # Editor Framework

        When writing neovim plugins or editing with `.vimrc.lua`, see [the guide](${coreEditorDoc}).
      '';

      settings = {
        includeCoAuthoredBy = false;
        theme = "dark";
        autoUpdates = false;

        permissions = {
          allow = [
            # Mostly read-only, but dangerously allows reading outside the workdir.
            "Bash(cat:*)"
            "Bash(date:*)"
            "Bash(file:*)"
            "Bash(find:*)"
            "Bash(grep:*)"
            "Bash(head:*)"
            "Bash(ls:*)"
            "Bash(mkdir:*)"
            "Bash(pwd:*)"
            "Bash(rg:*)"
            "Bash(tail:*)"
            "Bash(tree:*)"
            "Bash(wc:*)"
            "Bash(which:*)"

            "Bash(git add:*)"
            "Bash(git bisect:*)"
            "Bash(git blame:*)"
            "Bash(git commit:*)"
            "Bash(git diff:*)"
            "Bash(git fetch:*)"
            "Bash(git grep:*)"
            "Bash(git log:*)"
            "Bash(git ls-remote:*)"
            "Bash(git remote show:*)"
            "Bash(git restore:*)"
            "Bash(git show:*)"
            "Bash(git cherry-pick:*)"
            "Bash(git clean:*)"
            "Bash(git merge:*)"
            "Bash(git rebase:*)"
            "Bash(git reset:*)"
            "Bash(git revert:*)"
            "Bash(git stash:*)"
            "Bash(git status:*)"
            "Bash(git checkout:*)"
            "Bash(git branch:*)"
            "Bash(git tag:*)"

            "Bash(gh issue list:*)"
            "Bash(gh issue view:*)"
            "Bash(gh pr checks:*)"
            "Bash(gh pr diff:*)"
            "Bash(gh pr list:*)"
            "Bash(gh pr view:*)"
            "Bash(gh release list:*)"
            "Bash(gh release view:*)"
            "Bash(gh repo list:*)"
            "Bash(gh repo view:*)"
            "Bash(gh run list:*)"
            "Bash(gh run view:*)"
            "Bash(gh status:*)"
            "Bash(gh workflow list:*)"
            "Bash(gh workflow view:*)"

            # Rust Projects
            "Bash(cargo --list:*)"
            "Bash(cargo build:*)"
            "Bash(cargo check:*)"
            "Bash(cargo clippy:*)"
            "Bash(cargo doc:*)"
            "Bash(cargo fmt:*)"
            "Bash(cargo run:*)" # Arbitrary code execution.
            "Bash(cargo search:*)"
            "Bash(cargo test:*)" # Arbitrary code execution.
            "Bash(rustc:*)"

            # JavaScript / TypeScript projects
            "Bash(npm info:*)"
            "Bash(npm run)"
            "Bash(npm run:*)" # Arbitrary code execution.
            "Bash(npm test:*)"

            # Nix projects
            "Bash(nix build:*)"
            "Bash(nix develop:*)" # Arbitrary code execution.
            "Bash(nix eval:*)"
            "Bash(nix flake check:*)"
            "Bash(nix flake metadata:*)"
            "Bash(nix flake show:*)"
            "Bash(nix flake update:*)"
            "Bash(nix run:*)" # Arbitrary code execution.
            "Bash(nix search:*)"

            # Lua projects
            "Bash(luacheck:*)"

            # Generic projects
            "Bash(just:*)" # Arbitrary code execution.

            "WebFetch(domain:docs.anthropic.com)"
            "WebFetch(domain:docs.claude.com)"
            "WebFetch(domain:platform.openai.com)"
          ];
        };
      };
    };

    programs.git = lib.mkIf cfg.enable {
      ignores = [ "**/.claude/settings.local.json" ];
    };
  };
}
