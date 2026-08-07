{
  config,
  lib,
  pkgs,
  ...
}:

# Replaces home-manager's `programs.claude-code`. We use almost none of it and
# fight the rest: its plugin support wraps the binary (which breaks Claude's CLI
# flag parsing outright), its `marketplaces` option overwrites
# `settings.extraKnownMarketplaces` wholesale, and its `skills` rejects
# derivations. What's left is a thin mapping from options onto `home.file`,
# which is what this module is.

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

  marketplace = pkgs.linkFarm "claude-marketplace" (
    [
      {
        name = ".claude-plugin/marketplace.json";
        path = json.generate "marketplace.json" {
          name = "managed";
          owner.name = "Nix";
          plugins = lib.mapAttrsToList (name: plugin: {
            inherit name;
            inherit (plugin) description;
            source = "./plugins/${name}";
          }) cfg.plugins;
        };
      }
    ]
    ++ lib.concatLists (lib.mapAttrsToList pluginEntries cfg.plugins)
  );

  # `<root>/<subdir>/<name>.md` for each entry.
  markdownFiles =
    subdir:
    lib.mapAttrs' (
      name: source: lib.nameValuePair "${cfg.root}/${subdir}/${name}.md" { inherit source; }
    );
in

{
  disabledModules = [ "programs/claude-code.nix" ];

  options.programs.claude-code = {
    enable = lib.mkEnableOption "Claude Code, Anthropic's official CLI";

    package = lib.mkPackageOption pkgs "claude-code" { };

    root = lib.mkOption {
      type = lib.types.str;
      default = "${config.home.homeDirectory}/.claude";
      defaultText = lib.literalExpression ''"''${config.home.homeDirectory}/.claude"'';
      description = ''
        Directory holding Claude Code's configuration. {env}`CLAUDE_CONFIG_DIR`
        is exported whenever it differs from the CLI's own default.
      '';
    };

    settings = lib.mkOption {
      inherit (json) type;
      default = { };
      description = "Contents of {file}`settings.json`.";
    };

    context = lib.mkOption {
      type = lib.types.lines;
      default = "";
      description = "User-scoped memory, written to {file}`CLAUDE.md`.";
    };

    rules = lib.mkOption {
      type = lib.types.attrsOf lib.types.path;
      default = { };
      description = ''
        Path-scoped rule files, written to {file}`rules/<name>.md`. Every
        markdown file there is loaded as project memory.
      '';
    };

    commands = lib.mkOption {
      type = lib.types.attrsOf lib.types.path;
      default = { };
      description = "Slash-command prompts, written to {file}`commands/<name>.md`.";
    };

    skills = lib.mkOption {
      type = lib.types.attrsOf lib.types.path;
      default = { };
      description = ''
        Skill directories, linked to {file}`skills/<name>`. Each holds a
        `SKILL.md` plus whatever else the skill needs. Unlike home-manager's
        option this accepts derivations, so a skill can bundle generated files
        alongside its prompt.
      '';
    };

    keybindings = lib.mkOption {
      default = { };
      type = lib.types.attrsOf (lib.types.attrsOf (lib.types.nullOr lib.types.str));
      description = ''
        Keybinding overrides, written to {file}`keybindings.json`. Each
        attribute is a context whose value maps keys to commands. The schema
        field is added automatically. Bind a key to `null` to unbind it.
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

    plugins = lib.mkOption {
      default = { };
      description = ''
        Locally-defined plugins, published through an inline marketplace.

        Every plugin is provisioned *disabled*: the marketplace makes it
        available, and each repository enables the ones it wants through its own
        project settings. Upstream's plugin support is unusable here because it
        loads plugins by wrapping the `claude` binary, which breaks its CLI flag
        parsing.
      '';

      type = lib.types.attrsOf (
        lib.types.submodule {
          options = {
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

  config = lib.mkIf cfg.enable {
    # Deliberately unwrapped. Wrapping `claude` breaks its CLI flag parsing.
    home.packages = [ cfg.package ];

    home.sessionVariables = lib.mkIf (cfg.root != "${config.home.homeDirectory}/.claude") {
      CLAUDE_CONFIG_DIR = cfg.root;
    };

    programs.claude-code.settings = lib.mkIf (cfg.plugins != { }) {
      # Declared here rather than in `plugins/known_marketplaces.json`, which
      # Claude Code rewrites at runtime and cannot be a read-only store symlink.
      extraKnownMarketplaces.managed.source = {
        source = "directory";
        path = "${marketplace}";
      };

      enabledPlugins = lib.mapAttrs' (name: _: lib.nameValuePair "${name}@managed" false) cfg.plugins;
    };

    home.file = lib.mkMerge [
      (lib.mkIf (cfg.settings != { }) {
        "${cfg.root}/settings.json".source = json.generate "claude-code-settings.json" (
          cfg.settings
          // {
            "$schema" = "https://json.schemastore.org/claude-code-settings.json";
          }
        );
      })

      (lib.mkIf (cfg.context != "") {
        "${cfg.root}/CLAUDE.md".text = cfg.context;
      })

      (lib.mkIf (cfg.keybindings != { }) {
        "${cfg.root}/keybindings.json".source = json.generate "claude-code-keybindings.json" {
          "$schema" = "https://www.schemastore.org/claude-code-keybindings.json";
          bindings = keybindingsByContext;
        };
      })

      (markdownFiles "rules" cfg.rules)
      (markdownFiles "commands" cfg.commands)

      # `recursive` links each file inside the skill separately, so
      # `skills/<name>` stays a real directory. Without it the whole thing is
      # one symlink into the store, and nothing can be dropped alongside a
      # skill at runtime.
      (lib.mapAttrs' (
        name: source:
        lib.nameValuePair "${cfg.root}/skills/${name}" {
          inherit source;
          recursive = true;
        }
      ) cfg.skills)
    ];
  };
}
