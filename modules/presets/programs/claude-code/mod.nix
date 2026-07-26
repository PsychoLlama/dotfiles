{
  self,
  global,
  cfg,
  lib,
  pkgs,
  ...
}:

let
  agents = global."${self.agents}";
  trustedDirectories = global."${self.trusted-directories}".paths;

  inherit (self.presets.programs.claude-code) plugins;

  autoFormatHook = pkgs.callPackage ./hooks/auto-format.nix { };
  blockEnvFilesHook = pkgs.callPackage ./hooks/block-env-files.nix { };
  notifyPermissionRequestHook = pkgs.callPackage ./hooks/notify-permission-request.nix { };

  notifySkill = pkgs.callPackage ./skills/notify.nix { };
  statusline = pkgs.callPackage ./statusline.nix { };

  # A `hooks.<event>` matcher group that runs one command hook.
  commandHook = command: {
    hooks = [
      {
        type = "command";
        inherit command;
      }
    ];
  };
in

{
  options = {
    voice.package = lib.mkPackageOption pkgs.unstable "sox" {
      nullable = true;
    };
  };

  # The plugins are claude-code's own — they configure it and mean nothing
  # without it. Enabling them from here keeps them on by default while leaving
  # each one individually overridable.
  config = {
    "${plugins.chrome-devtools}".enable = lib.mkDefault true;
    "${plugins.lua-lsp}".enable = lib.mkDefault true;
    "${plugins.nil-lsp}".enable = lib.mkDefault true;
    "${plugins.nushell-lsp}".enable = lib.mkDefault true;
    "${plugins.rust-lsp}".enable = lib.mkDefault true;
    "${plugins.typescript-lsp}".enable = lib.mkDefault true;
  };

  platforms.home-manager =
    { config, ... }:

    let
      # Claude Code needs absolute prefixes; expand a leading `~` to the home dir.
      toAbsolute =
        dir:
        if lib.hasPrefix "~/" dir then "${config.home.homeDirectory}/${lib.removePrefix "~/" dir}" else dir;

      injectDirenvHook = pkgs.callPackage ./hooks/direnv.nix {
        direnv = config.programs.direnv.package;
      };
    in

    {
      programs.nushell.abbreviations.a = "claude"; # `a` short for `agent`

      home = {
        packages = lib.optionals (cfg.voice.package != null) [ cfg.voice.package ];

        # We want the skill directory to bundle SKILL.md alongside the `notify`
        # wrapper, but home-manager's `programs.claude-code.skills.<name>`
        # accepts only `lines` or a Nix `path`. Its `lib.isPath` branch rejects
        # derivation outputs (which are strings), so a `runCommand` that
        # assembles both files can't go through the option. Wire up `home.file`
        # directly instead.
        file = {
          ".claude/skills/notify/SKILL.md".source = ./skills/notify/SKILL.md;
          ".claude/skills/notify/notify".source = lib.getExe notifySkill;
        };
      };

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
            enabled = cfg.voice.package != null;
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

            additionalDirectories = map toAbsolute trustedDirectories;
          };

          statusLine = {
            type = "command";
            command = "${statusline}";
          };

          hooks = {
            PreToolUse = [
              (commandHook (lib.getExe blockEnvFilesHook) // { matcher = "Read|Edit|Write"; })
            ];

            PostToolUse = [ (commandHook autoFormatHook // { matcher = "Write|Edit"; }) ];

            Notification = [
              (commandHook (lib.getExe notifyPermissionRequestHook) // { matcher = "permission_prompt"; })
            ];
          }
          # The hook only makes sense when direnv is around to export anything.
          // lib.optionalAttrs config.programs.direnv.enable {
            SessionStart = [ (commandHook injectDirenvHook) ];
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
}
