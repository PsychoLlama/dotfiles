{ cfg, lib, ... }:

# Shared configuration for coding agents (Claude Code, codex, ...). This is
# *pure data*: it declares the canonical memory, rules, and skills once and
# holds nothing tool-specific. It configures no program and writes no files.
# Each agent preset reads it off `global` and decides how to render it into its
# own native shape.

let
  shared-options = {
    context = lib.mkOption {
      type = lib.types.lines;
      description = ''
        Global memory shared across agents. Rendered by each tool into its own
        memory surface (Claude Code's `CLAUDE.md` context, codex's session
        instructions, and so on).
      '';

      default = ''
        # Environment

        - Nix is installed with `nix-command flakes` enabled.
        - Prefer the `nix` command (`nix build` over `nix-build`, `nix shell` over `nix-shell`, etc).
        - Prefer `fd` over `find`.

        # Commit Messages

        - Imperative title, descriptive body (markdown).
        - Capture context not otherwise available (goal, failed approaches, decisions, etc).
      '';
    };

    rules = lib.mkOption {
      type = lib.types.attrsOf lib.types.path;
      description = ''
        Named rule files shared across agents, keyed by rule name. Each is a
        markdown file; content is authored in Claude Code's path-scoped rule
        format (YAML frontmatter). Tools without an equivalent mechanism may
        render a subset or ignore them.
      '';

      default = {
        authoring-agent-files = ./rules/authoring-agent-files.md;
        authoring-memory-files = ./rules/authoring-memory-files.md;
        neovim-development = ./rules/neovim-development.md;
        neovim-local-vimrc = ./rules/neovim-local-vimrc.md;
      };
    };

    skills = lib.mkOption {
      type = lib.types.attrsOf lib.types.path;
      description = ''
        Named skill directories shared across agents, keyed by skill name. Each
        points at a directory holding a `SKILL.md` (the format both Claude Code
        and codex load). Tool-specific skills stay in their own preset.
      '';

      default = {
        nushell-development = ./skills/nushell-development;
        using-nix = ./skills/using-nix;
      };
    };

    commands = lib.mkOption {
      type = lib.types.attrsOf lib.types.path;
      description = ''
        Named slash-command files shared across agents, keyed by command name.
        Each is a markdown prompt. Not every tool supports commands (codex has
        no analogue); those that don't simply ignore them.
      '';

      default = { };
    };
  };
in

{
  options = {
    # Data, not an effect. See `theme` for the reasoning.
    enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to publish the data to platforms that still read `psychollama.agents`.";
    };
  }
  // shared-options;

  # Transitional. The claude-code preset still reads `psychollama.agents` from
  # its own eval, and nothing declares those options there any more. A
  # foreign-class fragment is a full deferred module, so it carries the
  # declarations along with the values. Delete when claude-code migrates.
  platforms.home-manager = {
    options.psychollama.agents = shared-options;

    config.psychollama.agents = {
      inherit (cfg)
        context
        rules
        skills
        commands
        ;
    };
  };
}
