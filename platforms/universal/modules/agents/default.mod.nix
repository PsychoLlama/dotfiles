{ lib, ... }:

# Shared configuration for coding agents (Claude Code, codex, ...). This is
# *pure data*: it declares the canonical memory, rules, and skills once and
# holds nothing tool-specific. It has no `config` side effects -- it configures
# no program and writes no files. Each agent preset reads `psychollama.agents.*`
# and decides how to render it into its own native shape.
#
# Defined in `universal` because it's the only module set evaluated inside every
# substrate, so both the codex preset (NixOS) and the claude-code preset (Home
# Manager) read the same values with no bridge.

{
  options.psychollama.agents = {
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
        nushell-development = ./rules/nushell-development.md;
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

      default = {
        repo-update = ./commands/repo-update.md;
      };
    };
  };
}
