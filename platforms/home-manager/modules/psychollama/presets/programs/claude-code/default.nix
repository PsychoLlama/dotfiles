{
  pkgs,
  lib,
  config,
  ...
}:

let
  cfg = config.psychollama.presets.programs.claude-code;

  # Send a desktop notification on demand
  notifyUser =
    pkgs.writers.writeDash "notify"
      # bash
      ''
        title="Claude Code"
        icon="dialog-information"

        while [ $# -gt 0 ]; do
          case "$1" in
            --title) title="$2"; shift 2 ;;
            --icon) icon="$2"; shift 2 ;;
            *) break ;;
          esac
        done

        if [ $# -eq 0 ]; then
          echo "Usage: notify [--title TITLE] [--icon ICON] <message>" >&2
          exit 1
        fi

        message="$*"

        case "$(uname)" in
          Darwin)
            osascript -e "display notification \"$message\" with title \"$title\""
            ;;
          *)
            ${pkgs.libnotify}/bin/notify-send \
              --urgency=normal \
              --icon="$icon" \
              "$title" \
              "$message"
            ;;
        esac
      '';

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

        # Git Conventions

        - Branch naming: `jesse.gibson/<ticket-id>/<slug>` (with ticket) or `jesse.gibson/<slug>` (without ticket).
        - Worktree naming: `<repo>@<slug>`.
        - Avoid the `-C` flag.

        # Capabilities

        - `${config.programs.claude-code.scripts.notify.path} <message>` sends a desktop notification. Use it to get my attention after completing a long-running task.
      '';

      skills = {
        using-neovim = ./skills/using-neovim;
        using-nix = ./skills/using-nix;
      };

      commands.repo-update = ./commands/repo-update.md;

      settings = {
        includeCoAuthoredBy = false;
        theme = "dark";
        autoUpdates = false;
        preferredNotifChannel = "terminal_bell";
        model = "sonnet";

        env = {
          # I don't want uncommitted memory affecting Claude's decisions.
          CLAUDE_CODE_DISABLE_AUTO_MEMORY = "1";
        };

        permissions = {
          defaultMode = "acceptEdits";

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
            "Bash(nvim:*)"
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
            "Bash(nix derivation show:*)"
            "Bash(nix develop:*)" # Arbitrary code execution.
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
            "Bash(nix repl:*)" # Arbitrary code execution.
            "Bash(nix run:*)" # Arbitrary code execution.
            "Bash(nix search:*)"
            "Bash(nix why-depends:*)"

            # Lua projects
            "Bash(luacheck:*)"

            # Generic projects
            "Bash(just:*)" # Arbitrary code execution.

            # System Management
            "Bash(journalctl:*)"
            "Bash(systemctl list-units:*)"
            "Bash(systemctl status:*)"

            # Nix store (read-only source-diving)
            "Read(//nix/store/**)"

            "WebFetch(domain:docs.anthropic.com)"
            "WebFetch(domain:docs.claude.com)"
            "WebFetch(domain:platform.openai.com)"
          ];
        };
      };

      scripts.notify = {
        source = notifyUser;
        allow = true;
      };

      servers.chrome-devtools = {
        settings = {
          command = "${pkgs.chrome-devtools-mcp}/bin/chrome-devtools-mcp";

          # Only necessary on NixOS. Wish this supported an env variable.
          args = lib.optionals pkgs.stdenv.isLinux [
            "--executablePath"
            "${config.programs.chromium.package}/bin/chromium"
          ];
        };

        permissions.allow = [
          "click"
          "close_page"
          "drag"
          "emulate"
          "evaluate_script"
          "fill"
          "fill_form"
          "get_console_message"
          "get_network_request"
          "handle_dialog"
          "hover"
          "list_console_messages"
          "list_network_requests"
          "list_pages"
          "navigate_page"
          "new_page"
          "performance_analyze_insight"
          "performance_start_trace"
          "performance_stop_trace"
          "press_key"
          "resize_page"
          "select_page"
          "take_screenshot"
          "take_snapshot"
          "upload_file"
          "wait_for"
        ];
      };
    };

    programs.git = lib.mkIf cfg.enable {
      ignores = [ "**/.claude/settings.local.json" ];
    };
  };
}
