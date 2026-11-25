# What every agent is told, whichever agent it is. Each tool reads these and
# renders them its own way.
{ rhizome, units, ... }:

let
  t = rhizome.types;
  inherit (units.platform) level;
in

rec {
  memory = rhizome.channel "agents.memory" {
    level = level.user;

    type.input = {
      # Global memory: Claude Code's `CLAUDE.md` context, codex's session
      # instructions, and so on.
      context = t.str;

      # Rule files by name. Markdown, in Claude Code's path-scoped rule
      # format. Tools without an equivalent may render a subset or ignore them.
      rules = t.attrs-of t.any;

      # Slash-command files by name. Not every tool supports commands.
      commands = t.default (t.attrs-of t.any) { };
    };

    merge = rhizome.merge.one;
    optional = true;
  };

  unit = rhizome.component "agents" [
    (memory {
      context = ''
        # Environment

        - Nix is installed with `nix-command flakes` enabled.
        - Prefer the `nix` command (`nix build` over `nix-build`, `nix shell` over `nix-shell`, etc).
        - Prefer `fd` over `find`.

        # Commit Messages

        - Imperative title, descriptive body (markdown).
        - Capture context not otherwise available (goal, failed approaches, decisions, etc).
      '';

      rules = {
        authoring-agent-files = ./rules/authoring-agent-files.md;
        authoring-memory-files = ./rules/authoring-memory-files.md;
        neovim-development = ./rules/neovim-development.md;
        neovim-local-vimrc = ./rules/neovim-local-vimrc.md;
      };
    })
  ];
}
