# What every agent is told, whichever agent it is. Each tool reads these and
# renders them its own way.
{
  lib,
  rhizome,
  units,
  self,
  define,
  ...
}:

let
  t = rhizome.types;
  inherit (units.platform) user;
in

{
  memory = define.fact {
    scope = user.scope;

    type = {
      # Global memory: Claude Code's `CLAUDE.md` context, codex's session
      # instructions, and so on.
      context = t.str;

      # Rule files by name. Markdown, in Claude Code's path-scoped rule
      # format. Tools without an equivalent may render a subset or ignore them.
      rules = t.attrs-of t.path;

      # Slash-command files by name. Not every tool supports commands.
      commands = t.default (t.attrs-of t.path) { };
    };

    allow-unread = true;
  };

  # Something an agent can be taught to do. Each tool renders it its own way.
  skill = define.collection {
    scope = user.scope;
    allow-unread = true;

    type = {
      # Written to frontmatter and used as its directory name.
      name = t.str;

      # When the agent should use this skill.
      description = t.str;

      # Markdown body without frontmatter. A minijinja template.
      body = t.path;

      # Agents for which this skill is provisioned.
      targets = t.default (t.list t.str) [
        "claude"
        "codex"
      ];

      # More YAML frontmatter fields.
      frontmatter = t.default t.attrs { };

      # Template variables by name, then by agent.
      vars = t.default (t.attrs-of t.attrs) { };

      # Files or directories linked alongside `SKILL.md`: shared paths, or
      # paths by agent. Given `pkgs` if it is a function.
      files = t.default t.any { };
    };
  };

  lib = {
    /**
      The skills written for one agent, rendered, by name.

      # Type

      ```
      skills :: Pkgs -> String -> [Skill] -> { [String] :: Package }
      ```
    */
    skills =
      pkgs: agent: skills:
      lib.listToAttrs (
        map (
          skill:
          lib.nameValuePair skill.name (
            pkgs.callPackage ./_render.nix {
              inherit agent;

              skill = {
                inherit (skill) name body;
                files = if lib.isFunction skill.files then skill.files pkgs else skill.files;
                frontmatter = {
                  inherit (skill) name description;
                }
                // skill.frontmatter;

                vars = {
                  SKILL_DIRECTORY = {
                    claude = "\${CLAUDE_SKILL_DIR}";
                    codex = "~/.agents/skills/${skill.name}";
                  };
                }
                // skill.vars;
              };
            }
          )
        ) (lib.filter (skill: lib.elem agent skill.targets) skills)
      );
  };

  unit = define.unit {
    provides = [
      (self.memory {
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
  };
}
