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
    programs.claude-code = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.unstable.claude-code;
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
            "Bash(git status:*)"

            "Bash(gh issue list:*)"
            "Bash(gh issue view:*)"
            "Bash(gh pr checks:*)"
            "Bash(gh pr diff:*)"
            "Bash(gh pr list:*)"
            "Bash(gh pr view:*)"
            "Bash(gh repo list:*)"
            "Bash(gh repo view:*)"
            "Bash(gh status:*)"

            # Rust Projects
            "Bash(cargo build:*)"
            "Bash(cargo check:*)"
            "Bash(cargo clippy:*)"
            "Bash(cargo test:*)" # Potential arbitrary code execution.

            # JavaScript / TypeScript projects
            "Bash(npm info:*)"
            "Bash(npm run build:*)"
            "Bash(npm run fmt:*)"
            "Bash(npm run lint:*)"
            "Bash(npm run test:*)" # Potential arbitrary code execution.
            "Bash(npm run)"
            "Bash(npm test:*)"

            # Nix projects
            "Bash(nix eval:*)"
            "Bash(nix flake show:*)"
            "Bash(nix search:*)"

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
