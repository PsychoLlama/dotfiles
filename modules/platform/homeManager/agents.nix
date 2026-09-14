{
  flake.homeModules.platform =
    {
      lib,
      pkgs,
      ...
    }:

    let
      perAgent =
        type:
        lib.types.submodule {
          options = lib.genAttrs [ "claude" "codex" ] (
            agent:
            lib.mkOption {
              inherit type;
              default = throw "No value defined for agent '${agent}'.";
              description = "Value for ${agent}.";
            }
          );
        };

      skill = lib.types.submodule (
        { config, name, ... }: {
          options = {
            targets = lib.mkOption {
              type = lib.types.listOf (
                lib.types.enum [
                  "claude"
                  "codex"
                ]
              );
              default = [
                "claude"
                "codex"
              ];
              description = "Agents for which this skill is provisioned.";
            };

            name = lib.mkOption {
              type = lib.types.str;
              default = name;
              description = "Skill name written to frontmatter and used as its directory name.";
            };

            description = lib.mkOption {
              type = lib.types.str;
              description = "When the agent should use this skill.";
            };

            frontmatter = lib.mkOption {
              type = lib.types.attrsOf (pkgs.formats.yaml { }).type;
              default = { };
              description = "YAML frontmatter fields for the skill.";
            };

            template.body = lib.mkOption {
              type = lib.types.path;
              description = "Markdown body without frontmatter.";
            };

            template.vars = lib.mkOption {
              type = lib.types.attrsOf (perAgent lib.types.str);
              default = { };
              description = "Template variables indexed by variable name, then agent name.";
            };

            files = lib.mkOption {
              type = lib.types.attrsOf (lib.types.either lib.types.path (perAgent lib.types.path));
              default = { };
              description = "Files or directories linked alongside SKILL.md, as shared paths or paths indexed by agent.";
            };

            output = lib.mkOption {
              type = perAgent lib.types.package;
              readOnly = true;
              description = "Rendered skill packages indexed by agent.";
              default = lib.genAttrs [ "claude" "codex" ] (
                agent:
                pkgs.callPackage ./_agents/render.nix {
                  inherit agent;
                  skill = config;
                }
              );
            };
          };

          config = {
            frontmatter = {
              name = lib.mkDefault config.name;
              description = lib.mkDefault config.description;
            };

            template.vars.SKILL_DIRECTORY = {
              claude = lib.mkDefault "\${CLAUDE_SKILL_DIR}";
              codex = lib.mkDefault "~/.agents/skills/${config.name}";
            };
          };
        }
      );
    in

    {
      options.agents = {
        skills = lib.mkOption {
          type = lib.types.attrsOf skill;
          default = { };
          description = "Shared skill definitions and their rendered packages.";
        };
      };
    };
}
