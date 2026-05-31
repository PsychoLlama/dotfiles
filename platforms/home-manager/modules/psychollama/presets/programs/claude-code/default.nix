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
    home.shellAliases.a = "claude"; # `a` short for `agent`

    programs.claude-code = {
      enable = lib.mkDefault true;
      package = lib.mkDefault pkgs.unstable.claude-code-bin;

      context = ''
        # Environment

        - Nix is installed with `nix-command flakes` enabled.
        - Prefer the `nix` command (`nix build` over `nix-build`, `nix shell` over `nix-shell`, etc).
        - Prefer `fd` over `find`.
        - Prefer `rg` over `grep`.

        # Commit Messages

        - Imperative title, descriptive body (markdown).
        - Capture context not otherwise available (goal, failed approaches, decisions, etc).

        # Authoring Agent Files

        - `AGENTS.md` is the source of truth. `CLAUDE.md` should only contain `@AGENTS.md`.
        - Use short, declarative statements or lists of instructions.
        - Each item should be 2 sentences or less.
        - Use these guidelines when authoring skills, commands, or other files intended solely for agents.
      '';

      skills = {
        codex-review = ./skills/codex-review;
        using-neovim = ./skills/using-neovim;
        using-nix = ./skills/using-nix;
      };

      commands.repo-update = ./commands/repo-update.md;

      settings = {
        theme = "dark";
        preferredNotifChannel = "terminal_bell";

        # settings.json is a read-only symlink into the Nix store, so Claude
        # Code's "don't ask again" toggle for auto mode can never persist.
        # Opt out of the confirmation up front.
        skipAutoPermissionPrompt = true;

        attribution = {
          commit = "";
          pr = "";
        };

        env = {
          # I don't want uncommitted memory affecting Claude's decisions.
          CLAUDE_CODE_DISABLE_AUTO_MEMORY = "1";
        };

        worktree.symlinkDirectories = [ ".direnv" ];

        permissions = {
          defaultMode = "auto";

          additionalDirectories = [
            "${config.home.homeDirectory}/projects/psychollama"
            "${config.home.homeDirectory}/projects/@learn"
            "${config.home.homeDirectory}/projects/retreon"
            "${config.home.homeDirectory}/projects/ambient-computer"
          ];
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
