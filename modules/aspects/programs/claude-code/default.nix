{
  imports = [
    ./hooks/auto-format.nix
    ./hooks/block-env-files.nix
    ./hooks/direnv.nix
    ./hooks/notify-permission-request.nix
    ./plugins/chrome-devtools.nix
    ./plugins/lua-lsp.nix
    ./plugins/nil-lsp.nix
    ./plugins/nushell-lsp.nix
    ./plugins/rust-lsp.nix
    ./plugins/typescript-lsp.nix
    ./skills/notify.nix
    ./statusline.nix
  ];

  exports.homeManager =
    {
      pkgs,
      lib,
      config,
      host,
      ...
    }:

    let
      inherit (host) agents trusted-directories;

      # Claude Code needs absolute prefixes; expand a leading `~` to the home dir.
      toAbsolute =
        dir:
        if lib.hasPrefix "~/" dir then "${config.home.homeDirectory}/${lib.removePrefix "~/" dir}" else dir;
    in

    {
      config = {
        programs.nushell.abbreviations.a = "claude"; # `a` short for `agent`

        programs.claude-code = {
          enable = lib.mkDefault true;
          package = lib.mkDefault pkgs.unstable.custom.claude-code-bin;

          # Shared stuff across all agent tools.
          inherit (agents) rules context commands;
          skills = agents.skills // {
            codex-review = ./skills/codex-review;
          };

          keybindings.Chat = {
            # Default keybind toggles fast mode. Emulator interprets `<esc>o`
            # as `meta+o`, conflicting with vim mode if I'm typing quickly.
            "meta+o" = "chat:newline";
          };

          settings = {
            theme = "dark";
            preferredNotifChannel = "terminal_bell";

            # settings.json is a read-only symlink into the Nix store, so Claude
            # Code's "don't ask again" toggle for auto mode can never persist.
            # Opt out of the confirmation up front.
            skipAutoPermissionPrompt = true;

            # Terrible idea and never should've been added.
            disableDeepLinkRegistration = "disable";

            # Too repetitive.
            spinnerTipsEnabled = false;

            # Default style narrates too much.
            outputStyle = "Concise";

            # Offensive.
            autoInstallIdeExtension = false;

            # Combat the doorway effect.
            externalEditorContext = true;

            # Plan iteration munches all my tokens.
            showClearContextOnPlanAccept = true;

            # I don't want uncommitted memory affecting Claude's decisions.
            autoMemoryEnabled = false;

            # Default only seems to render what's in the viewport. Tmux sadness.
            tui = "fullscreen";

            # What could go wrong.
            remoteControlAtStartup = true;

            # I never want these by default.
            disableClaudeAiConnectors = true;

            # I'm not their advertising billboard.
            attribution = {
              commit = "";
              pr = "";
              sessionUrl = false;
            };

            worktree = {
              baseRef = "head";
              symlinkDirectories = [
                ".claude/settings.local.json"
                ".direnv"
                ".envrc"
                ".vimrc.lua"
              ];
            };

            permissions = {
              defaultMode = "auto";

              additionalDirectories = map toAbsolute trusted-directories;
            };
          };
        };

        programs.git = {
          ignores = [
            "**/.claude/*.lock"
            "**/.claude/settings.local.json"
            "**/.claude/worktrees"
            "**/CLAUDE.local.md"
          ];
        };
      };
    };
}
