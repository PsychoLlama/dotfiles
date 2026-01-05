{
  pkgs,
  lib,
  config,
  ...
}:

let
  cfg = config.psychollama.presets.programs.claude-code;
  coreEditorDoc = pkgs.writeText "neovim-framework.md" (builtins.readFile ./neovim-framework.md);

  # Block access to files named exactly ".env"
  blockEnvFiles = pkgs.writers.writeDash "block-env-files" ''
    file_path=$(${pkgs.jq}/bin/jq -r '.tool_input.file_path // ""')
    basename=$(basename "$file_path")

    if [ "$basename" = ".env" ]; then
      echo "Access to .env files is blocked" >&2
      exit 2
    fi
  '';
in

{
  options.psychollama.presets.programs.claude-code = {
    enable = lib.mkEnableOption "Opinionated config for Claude Code";
  };

  config = lib.mkIf cfg.enable {
    # Load Claude MCP tool definitions on-demand. Saves on context utilization.
    home.sessionVariables.ENABLE_EXPERIMENTAL_MCP_CLI = "true";

    programs.claude-code = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.unstable.claude-code;

      mcpServers = {
        chrome-devtools = {
          command = "${pkgs.chrome-devtools-mcp}/bin/chrome-devtools-mcp";

          # Only necessary on NixOS. Wish this supported an env variable.
          args = lib.optionals pkgs.stdenv.isLinux [
            "--executablePath"
            "${config.programs.chromium.package}/bin/chromium"
          ];
        };
      };

      commands = {
        "quality.plan" = ./commands/quality.plan.md;
        "quality.run" = ./commands/quality.run.md;
      };

      memory.text = ''
        # Environment

        - Nix is installed with `nix-command flakes` enabled.
        - Prefer the `nix` command (`nix build` over `nix-build`, `nix shell` over `nix-shell`, etc).

        # Developing Neovim Plugins

        - Use `nvim --headless -c 'help <name> | echo expand("%:p") | qa'` to find plugin help pages.
        - Use `nvim --headless -c 'echo $VIMRUNTIME | qa'` to find the neovim runtime.
        - When editing `.vimrc.lua`, see [the core library guide](${coreEditorDoc}).

        # Nix Codebases

        - Use `nix eval` and `nix build` to experiment with your changes.
        - New files are not discoverable by Nix until you `git add` them.
        - Dotfiles repos discover and import `.nix` files automatically. No need for module `imports`.
      '';

      settings = {
        includeCoAuthoredBy = false;
        theme = "dark";
        autoUpdates = false;
        preferredNotifChannel = "terminal_bell";
        model = "opus";

        enabledPlugins = {
          "ralph-wiggum@claude-plugins-official" = lib.mkDefault true;
        };

        hooks = {
          PreToolUse = [
            {
              matcher = "Read|Edit|Write";
              hooks = [
                {
                  type = "command";
                  command = blockEnvFiles;
                }
              ];
            }
          ];

          Stop = [
            {
              matcher = "*";
              hooks = [
                {
                  type = "command";
                  command = ./hooks/keep-going.nu;
                }
              ];
            }
          ];
        };

        permissions = {
          defaultMode = "acceptEdits";
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

            # System Management
            "Bash(journalctl:*)"
            "Bash(systemctl list-units:*)"
            "Bash(systemctl status:*)"

            "WebFetch(domain:docs.anthropic.com)"
            "WebFetch(domain:docs.claude.com)"
            "WebFetch(domain:platform.openai.com)"

            # Chrome DevTools MCP
            # DANGER! Assumes an isolated browser.
            "mcp__chrome-devtools__click"
            "mcp__chrome-devtools__close_page"
            "mcp__chrome-devtools__drag"
            "mcp__chrome-devtools__emulate"
            "mcp__chrome-devtools__evaluate_script"
            "mcp__chrome-devtools__fill"
            "mcp__chrome-devtools__fill_form"
            "mcp__chrome-devtools__get_console_message"
            "mcp__chrome-devtools__get_network_request"
            "mcp__chrome-devtools__handle_dialog"
            "mcp__chrome-devtools__hover"
            "mcp__chrome-devtools__list_console_messages"
            "mcp__chrome-devtools__list_network_requests"
            "mcp__chrome-devtools__list_pages"
            "mcp__chrome-devtools__navigate_page"
            "mcp__chrome-devtools__new_page"
            "mcp__chrome-devtools__performance_analyze_insight"
            "mcp__chrome-devtools__performance_start_trace"
            "mcp__chrome-devtools__performance_stop_trace"
            "mcp__chrome-devtools__press_key"
            "mcp__chrome-devtools__resize_page"
            "mcp__chrome-devtools__select_page"
            "mcp__chrome-devtools__take_screenshot"
            "mcp__chrome-devtools__take_snapshot"
            "mcp__chrome-devtools__upload_file"
            "mcp__chrome-devtools__wait_for"
          ];
        };
      };
    };

    programs.git = lib.mkIf cfg.enable {
      ignores = [ "**/.claude/settings.local.json" ];
    };
  };
}
