{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.psychollama) agents;
  cfg = config.psychollama.presets.programs.codex;

  trustedDirectoriesHook = pkgs.callPackage ./hooks/trusted-directories.nix {
    directories = config.psychollama.trusted-directories;
  };

  localInstructionsHook = pkgs.callPackage ./hooks/local-instructions.nix { };

  autoFormatHook = pkgs.callPackage ./hooks/auto-format.nix { };

  # A `hooks.SessionStart` matcher group that runs one command hook.
  commandHook = command: {
    hooks = [
      {
        type = "command";
        command = lib.getExe command;
      }
    ];
  };

  settingsFormat = pkgs.formats.toml { };

  settings = {
    # Not well supported by TOML, but hey, might as well try.
    "$schema" = "https://learn.chatgpt.com/docs/config-schema.json";

    # Everything must be vim.
    tui.vim_mode_default = true;

    # Updates are managed by Nix.
    check_for_update_on_startup = false;

    # Privacy.
    analytics.enabled = false;
    feedback.enabled = false;

    # Default tries VS Code.
    file_opener = "none";

    # Memories are a source of hidden, uncommitted behavior. Not a fan.
    features.memories = false;

    # Poor-man's equivalent of `~/.claude/CLAUDE.md`.
    #
    # Codex has real support, but since this is a NixOS profile, we can't do
    # per-user home files. Sad.
    developer_instructions = agents.context;

    hooks.SessionStart =
      # Inject CLAUDE.local.md into context, the way Claude Code does.
      [ (commandHook localInstructionsHook) ]
      # Seed trust for repos under my trusted directories, so codex stops
      # prompting in repos I already own. Only wired when there's something to
      # trust, which also guarantees the hook always has a search path (see the
      # hook for why).
      ++ lib.optional (config.psychollama.trusted-directories != [ ]) (
        commandHook trustedDirectoriesHook
      );

    # Format edited files after `apply_patch`, the way Claude Code's auto-format
    # hook does. Codex surfaces `apply_patch` under the `Write`/`Edit` matcher
    # aliases, so this matcher mirrors the Claude preset's.
    hooks.PostToolUse = [ (commandHook autoFormatHook // { matcher = "Edit|Write"; }) ];
  };
in

{
  options.psychollama.presets.programs.codex = {
    enable = lib.mkEnableOption "Install the latest version of codex";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.unstable.custom.codex-bin;
      defaultText = lib.literalExpression "pkgs.unstable.custom.codex-bin";
      description = "The codex package to install.";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [ cfg.package ];

    # User config is left writable and untracked because codex *insists* on
    # mutating it. So we provision a system-level config instead.
    environment.etc."codex/config.toml".source = settingsFormat.generate "codex-config.toml" settings;
  };
}
