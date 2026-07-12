{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.codex;

  trustedDirectoriesHook = pkgs.callPackage ./hooks/trusted-directories.nix {
    directories = config.psychollama.trusted-directories;
  };

  localInstructionsHook = pkgs.callPackage ./hooks/local-instructions.nix { };

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
  };
in

{
  options.psychollama.presets.programs.codex = {
    enable = lib.mkEnableOption "Install the latest version of codex";

    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.unstable.codex;
      defaultText = lib.literalExpression "pkgs.unstable.codex";
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
