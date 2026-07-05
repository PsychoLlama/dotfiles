{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.claude-code;
  json = pkgs.formats.json { };

  keybindingsByContext = lib.mapAttrsToList (context: bindings: {
    inherit context bindings;
  }) cfg.keybindings;

  pluginEntries =
    name: plugin:
    [
      {
        name = "plugins/${name}/.claude-plugin/plugin.json";
        path = json.generate "plugin.json" {
          inherit name;
          inherit (plugin) description;
        };
      }
    ]
    ++ lib.optional (plugin.lsp.servers != { }) {
      name = "plugins/${name}/.lsp.json";
      path = json.generate "lsp.json" plugin.lsp.servers;
    }
    ++ lib.optional (plugin.mcp.servers != { }) {
      name = "plugins/${name}/.mcp.json";
      path = json.generate "mcp.json" plugin.mcp.servers;
    };

  marketplace = pkgs.linkFarm "claude-marketplace-dotfiles" (
    [
      {
        name = ".claude-plugin/marketplace.json";
        path = json.generate "marketplace.json" {
          name = "dotfiles";
          owner.name = "dotfiles";
          plugins = lib.mapAttrsToList (name: plugin: {
            inherit name;
            inherit (plugin) description;
            source = "./plugins/${name}";
          }) cfg.localPlugins;
        };
      }
    ]
    ++ lib.concatLists (lib.mapAttrsToList pluginEntries cfg.localPlugins)
  );
in

{
  options.programs.claude-code = {
    keybindings = lib.mkOption {
      default = { };
      type = lib.types.attrsOf (lib.types.attrsOf (lib.types.nullOr lib.types.str));
      description = ''
        Keybinding overrides for Claude Code, rendered to
        {file}`keybindings.json` inside {option}`programs.claude-code.configDir`
        (default {file}`~/.claude/keybindings.json`). Each attribute is a
        context whose value maps keys to commands. The schema field is added
        automatically. Bind a key to `null` to unbind it.
      '';

      example = lib.literalExpression ''
        {
          Chat = {
            "ctrl+e" = "chat:externalEditor";
            "ctrl+u" = null;
          };
        }
      '';
    };

    localPlugins = lib.mkOption {
      default = { };
      description = ''
        Locally-defined Claude Code plugins. Generates an inline settings
        marketplace and enables each plugin via settings.json. Distinct from
        the upstream `plugins` option, which loads external plugin directories.
      '';

      type = lib.types.attrsOf (
        lib.types.submodule {
          options = {
            enable = lib.mkOption {
              type = lib.types.bool;
              default = false;
              description = ''
                Default enablement for this plugin. Every plugin is published
                to the marketplace and provisioned in settings.json regardless;
                this only sets the default value, which can be overridden
                per-project through project-level settings.
              '';
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
  };

  config = lib.mkIf cfg.enable (
    lib.mkMerge [
      (lib.mkIf (cfg.keybindings != { }) {
        home.file."${cfg.configDir}/keybindings.json".source =
          json.generate "claude-code-keybindings.json"
            {
              "$schema" = "https://www.schemastore.org/claude-code-keybindings.json";
              bindings = keybindingsByContext;
            };
      })

      (lib.mkIf (cfg.localPlugins != { }) {
        programs.claude-code.settings = {
          extraKnownMarketplaces.dotfiles.source = {
            source = "directory";
            path = "${marketplace}";
          };

          enabledPlugins = lib.mapAttrs' (
            name: plugin: lib.nameValuePair "${name}@dotfiles" plugin.enable
          ) cfg.localPlugins;
        };
      })
    ]
  );
}
