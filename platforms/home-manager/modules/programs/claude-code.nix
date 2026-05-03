{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.claude-code;
  json = pkgs.formats.json { };

  rootDir = ".claude/dotfiles";
  scriptsDir = "${rootDir}/bin";

  enabledPlugins = lib.filterAttrs (_: plugin: plugin.enable) cfg.plugins;

  # HACK: Assembling directory trees with runCommand + mkdir/ln is ugly.
  # Replace with a proper declarative directory builder (e.g. linkFarm,
  # pkgs.buildEnv, or a custom lib helper) when one fits.
  mkPluginDir =
    name: plugin:
    pkgs.runCommand "claude-plugin-${name}" { } ''
      mkdir -p $out/.claude-plugin
      ln -s ${
        json.generate "plugin.json" {
          inherit name;
          inherit (plugin) description;
        }
      } $out/.claude-plugin/plugin.json
      ${lib.optionalString (plugin.lsp.servers != { }) ''
        ln -s ${json.generate "lsp.json" plugin.lsp.servers} $out/.lsp.json
      ''}
      ${lib.optionalString (plugin.mcp.servers != { }) ''
        ln -s ${json.generate "mcp.json" plugin.mcp.servers} $out/.mcp.json
      ''}
    '';

  marketplace = pkgs.runCommand "claude-marketplace-dotfiles" { } ''
    mkdir -p $out/.claude-plugin $out/plugins
    ln -s ${
      json.generate "marketplace.json" {
        name = "dotfiles";
        owner.name = "dotfiles";
        plugins = lib.mapAttrsToList (name: plugin: {
          inherit name;
          inherit (plugin) description;
          source = "./plugins/${name}";
        }) enabledPlugins;
      }
    } $out/.claude-plugin/marketplace.json
    ${lib.concatStringsSep "\n" (
      lib.mapAttrsToList (
        name: plugin: "ln -s ${mkPluginDir name plugin} $out/plugins/${name}"
      ) enabledPlugins
    )}
  '';
in

{
  options.programs.claude-code = {
    scripts = lib.mkOption {
      default = { };
      description = "Executable scripts installed at ~/${scriptsDir}/<name>. The attribute name becomes the script name.";
      type = lib.types.attrsOf lib.types.path;
    };

    plugins = lib.mkOption {
      default = { };
      description = ''
        Claude Code plugins. Generates an inline settings marketplace and
        enables each plugin via settings.json.
      '';

      type = lib.types.attrsOf (
        lib.types.submodule {
          options = {
            enable = lib.mkOption {
              type = lib.types.bool;
              default = true;
              description = "Whether to enable this plugin.";
            };

            description = lib.mkOption {
              type = lib.types.str;
              default = "";
              description = "Brief description of the plugin.";
            };

            lsp.servers = lib.mkOption {
              type = json.type;
              default = { };
              description = "LSP server configurations (rendered to .lsp.json at the plugin root).";
            };

            mcp.servers = lib.mkOption {
              type = json.type;
              default = { };
              description = "MCP server configurations (rendered to .mcp.json at the plugin root).";
            };
          };
        }
      );
    };

    agentManifest = lib.mkOption {
      default = { };
      description = ''
        Agents passed via `claude --agents`. The attribute name becomes the agent name.
        Generates a JSON file at ~/${rootDir}/share/agents.json.

        Useful for defining agents Claude can't invoke, but still accessible
        with `--agent <name>`. For example: a research agent with blanket web
        access.
      '';

      # Adapted from: https://code.claude.com/docs/en/cli-reference#agents-flag-format
      type = lib.types.attrsOf (
        lib.types.submodule {
          options = {
            description = lib.mkOption {
              type = lib.types.str;
              description = "When Claude should delegate to this agent.";
            };

            prompt = lib.mkOption {
              type = lib.types.nullOr lib.types.str;
              default = null;
              description = "System prompt for the agent.";
            };

            tools = lib.mkOption {
              type = lib.types.nullOr (lib.types.listOf lib.types.str);
              default = null;
              description = "Tools the agent can use. Inherits all tools if null.";
            };

            disallowedTools = lib.mkOption {
              type = lib.types.nullOr (lib.types.listOf lib.types.str);
              default = null;
              description = "Tools to deny.";
            };

            model = lib.mkOption {
              type = lib.types.nullOr (
                lib.types.enum [
                  "sonnet"
                  "opus"
                  "haiku"
                  "inherit"
                ]
              );
              default = null;
              description = "Model to use. Defaults to inherit.";
            };

            permissionMode = lib.mkOption {
              type = lib.types.nullOr (
                lib.types.enum [
                  "default"
                  "acceptEdits"
                  "dontAsk"
                  "bypassPermissions"
                  "plan"
                ]
              );
              default = null;
              description = "Permission mode for the agent.";
            };

            maxTurns = lib.mkOption {
              type = lib.types.nullOr lib.types.int;
              default = null;
              description = "Maximum agentic turns before the agent stops.";
            };

            # skills, mcpServers, and hooks are also supported by --agents
            # but are complex structures left for future implementation.

            memory = lib.mkOption {
              type = lib.types.nullOr (
                lib.types.enum [
                  "user"
                  "project"
                  "local"
                ]
              );
              default = null;
              description = "Persistent memory scope.";
            };
          };
        }
      );
    };
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      # Scripts: install to dotfiles bin dir
      {
        home.file = lib.mapAttrs' (
          name: source: lib.nameValuePair "${scriptsDir}/${name}" { inherit source; }
        ) cfg.scripts;
      }

      # Plugins: generate a local directory marketplace
      (lib.mkIf (enabledPlugins != { }) {
        programs.claude-code.settings = {
          extraKnownMarketplaces.dotfiles.source = {
            source = "directory";
            path = "${marketplace}";
          };

          enabledPlugins = lib.mapAttrs' (name: _: lib.nameValuePair "${name}@dotfiles" true) enabledPlugins;
        };
      })

      # Agent manifest: generate agents.json for use with --agents
      (lib.mkIf (cfg.agentManifest != { }) {
        home.file."${rootDir}/share/agents.json".source = json.generate "agents.json" (
          lib.mapAttrs (_: agent: lib.filterAttrs (_: v: v != null) agent) cfg.agentManifest
        );
      })
    ]
  );
}
