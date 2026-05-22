{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.claude-code;
  json = pkgs.formats.json { };

  enabledPlugins = lib.filterAttrs (_: plugin: plugin.enable) cfg.plugins;

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
          }) enabledPlugins;
        };
      }
    ]
    ++ lib.concatLists (lib.mapAttrsToList pluginEntries enabledPlugins)
  );
in

{
  options.programs.claude-code = {
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

  };

  config = lib.mkIf (cfg.enable && enabledPlugins != { }) {
    programs.claude-code.settings = {
      extraKnownMarketplaces.dotfiles.source = {
        source = "directory";
        path = "${marketplace}";
      };

      enabledPlugins = lib.mapAttrs' (name: _: lib.nameValuePair "${name}@dotfiles" true) enabledPlugins;
    };
  };
}
