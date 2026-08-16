{
  exports.nixos =
    {
      config,
      host,
      lib,
      pkgs,
      ...
    }:

    let
      inherit (host) agents;

      cfg = config.psychollama.presets.programs.codex;

      # `writers.writeNuBin` bakes in `pkgs.nushell`, but the nushell preset pins
      # `pkgs.unstable.nushell`, so the default writer drags a second nushell
      # (~57MB) into the closure. Rebuild the writer over the pinned interpreter.
      # This is a NixOS module, so the home-manager `programs.nushell.package`
      # isn't reachable from here; the preset's pin is the same value.
      writeNuBin =
        name:
        pkgs.writers.makeScriptWriter {
          interpreter = "${lib.getExe pkgs.unstable.nushell} --no-config-file";
        } "/bin/${name}";

      localInstructionsHook = pkgs.callPackage ./hooks/_local-instructions.nix {
        inherit writeNuBin;
      };

      autoFormatHook = pkgs.callPackage ./hooks/_auto-format.nix {
        nushell = pkgs.unstable.nushell;
      };

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

        # Inject CLAUDE.local.md into context, the way Claude Code does.
        hooks.SessionStart = [ (commandHook localInstructionsHook) ];

        # Format edited files after `apply_patch`, the way Claude Code's auto-format
        # hook does. Codex surfaces `apply_patch` under the `Write`/`Edit` matcher
        # aliases, so this matcher mirrors the Claude preset's.
        hooks.PostToolUse = [ (commandHook autoFormatHook // { matcher = "Edit|Write"; }) ];
      };
    in

    {
      options.psychollama.presets.programs.codex = {
        package = lib.mkOption {
          type = lib.types.package;
          default = pkgs.unstable.custom.codex-bin;
          defaultText = lib.literalExpression "pkgs.unstable.custom.codex-bin";
          description = "The codex package to install.";
        };
      };

      config = {
        environment.systemPackages = [ cfg.package ];

        # User config is left writable and untracked because codex *insists* on
        # mutating it. So we provision a system-level config instead.
        environment.etc."codex/config.toml".source = settingsFormat.generate "codex-config.toml" settings;
      };
    };
}
