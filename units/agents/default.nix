# What every agent is told, whichever agent it is. Each tool reads these and
# renders them its own way.
{
  lib,
  rhizome,
  units,
  ...
}:

let
  t = rhizome.types;
  inherit (units.platform) level;
in

rec {
  # Global memory shared across agents: Claude Code's `CLAUDE.md` context,
  # codex's session instructions, and so on.
  context = rhizome.channel "agents.context" {
    level = level.user;
    type.input = t.str;
    merge = entries: lib.concatStringsSep "\n" (map (entry: entry.value) entries);
    optional = true;
  };

  # Rule files by name. Markdown, in Claude Code's path-scoped rule format.
  # Tools without an equivalent may render a subset or ignore them.
  rules = rhizome.channel "agents.rules" {
    level = level.user;
    type.input = t.attrs-of t.any;
    merge = rhizome.merge.attrs.recursive;
    optional = true;
  };

  # Slash-command files by name. Not every tool supports commands.
  commands = rhizome.channel "agents.commands" {
    level = level.user;
    type.input = t.attrs-of t.any;
    merge = rhizome.merge.attrs.recursive;
    optional = true;
  };

  unit = rhizome.component "agents" [
    (context ''
      # Environment

      - Nix is installed with `nix-command flakes` enabled.
      - Prefer the `nix` command (`nix build` over `nix-build`, `nix shell` over `nix-shell`, etc).
      - Prefer `fd` over `find`.

      # Commit Messages

      - Imperative title, descriptive body (markdown).
      - Capture context not otherwise available (goal, failed approaches, decisions, etc).
    '')

    (rules {
      authoring-agent-files = ./rules/authoring-agent-files.md;
      authoring-memory-files = ./rules/authoring-memory-files.md;
      neovim-development = ./rules/neovim-development.md;
      neovim-local-vimrc = ./rules/neovim-local-vimrc.md;
    })
  ];
}
