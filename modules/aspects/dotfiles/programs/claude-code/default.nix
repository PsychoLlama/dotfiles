{ den, dotfiles, ... }:

{
  dotfiles.programs.claude-code = {
    # Den's synthetic `_` bundle only reaches one level below a namespace key,
    # so the plugins are too deeply nested to pull in as a group.
    includes = [
      (den.batteries.unfree [ "claude-code-bin" ])
      dotfiles.programs.claude-code.plugins.chrome-devtools
      dotfiles.programs.claude-code.plugins.lua-lsp
      dotfiles.programs.claude-code.plugins.nil-lsp
      dotfiles.programs.claude-code.plugins.nushell-lsp
      dotfiles.programs.claude-code.plugins.rust-lsp
      dotfiles.programs.claude-code.plugins.typescript-lsp
    ];

    homeManager =
      {
        config,
        lib,
        pkgs,
        ...
      }:

      let
        inherit (config.psychollama) agents;

        # Claude Code needs absolute prefixes; expand a leading `~` to the home dir.
        toAbsolute =
          dir:
          if lib.hasPrefix "~/" dir then "${config.home.homeDirectory}/${lib.removePrefix "~/" dir}" else dir;
      in

      {
        imports = [
          ./_hooks
          ./_skills.nix
          ./_statusline.nix
        ];

        programs.nushell.abbreviations.a = "claude"; # `a` short for `agent`

        home.packages = [ pkgs.unstable.sox ]; # Voice input.

        programs.claude-code = {
          enable = true;
          package = lib.mkDefault pkgs.unstable.custom.claude-code-bin;

          # Shared stuff across all agent tools.
          inherit (agents) rules context commands;

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

            voice = {
              enabled = true;
              mode = "hold";
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

              additionalDirectories = map toAbsolute config.psychollama.trusted-directories;
            };
          };
        };

        programs.git.ignores = [
          "**/.claude/*.lock"
          "**/.claude/settings.local.json"
          "**/.claude/worktrees"
          "**/CLAUDE.local.md"
        ];
      };
  };
}
