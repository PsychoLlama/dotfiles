{
  pkgs,
  lib,
  config,
  ...
}:

let
  inherit (config.psychollama) agents;
  cfg = config.psychollama.presets.programs.claude-code;

  # Claude Code needs absolute prefixes; expand a leading `~` to the home dir.
  toAbsolute =
    dir:
    if lib.hasPrefix "~/" dir then "${config.home.homeDirectory}/${lib.removePrefix "~/" dir}" else dir;
in

{
  options.psychollama.presets.programs.claude-code = {
    enable = lib.mkEnableOption "Opinionated config for Claude Code";

    voice.package = lib.mkPackageOption pkgs.unstable "sox" {
      nullable = true;
    };
  };

  config = lib.mkIf cfg.enable {
    programs.nushell.abbreviations.a = "claude"; # `a` short for `agent`

    home.packages = lib.optionals (cfg.voice.package != null) [ cfg.voice.package ];

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

        # I'm not their advertising billboard.
        attribution = {
          commit = "";
          pr = "";
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

          additionalDirectories = map toAbsolute config.psychollama.trusted-directories;
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
