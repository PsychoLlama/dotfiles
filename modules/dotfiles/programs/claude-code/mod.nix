{
  self,
  cfg,
  lib,
  pkgs,
  ...
}:

let
  agents = self.agents;
  trustedDirectories = self.trusted-directories.paths;

  json = pkgs.formats.json { };

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

  autoFormatHook = pkgs.callPackage ./hooks/auto-format.nix { };
  blockEnvFilesHook = pkgs.callPackage ./hooks/block-env-files.nix { };
  notifyPermissionRequestHook = pkgs.callPackage ./hooks/notify-permission-request.nix { };

  notifySkill = pkgs.callPackage ./skills/notify.nix { };
  statusline = pkgs.callPackage ./statusline.nix { };

  # A `hooks.<event>` matcher group that runs one command hook.
  commandHook = command: {
    hooks = [
      {
        type = "command";
        inherit command;
      }
    ];
  };
in

{
  options = {
    voice.package = lib.mkPackageOption pkgs.unstable "sox" {
      nullable = true;
    };
  };

  # The plugins are claude-code's own — they configure it and mean nothing
  # without it. Enabling them from here keeps them on by default while leaving
  # each one individually overridable.
  config.programs.claude-code.plugins = {
    chrome-devtools.enable = lib.mkDefault true;
    lua-lsp.enable = lib.mkDefault true;
    nil-lsp.enable = lib.mkDefault true;
    nushell-lsp.enable = lib.mkDefault true;
    rust-lsp.enable = lib.mkDefault true;
    typescript-lsp.enable = lib.mkDefault true;
  };

  config.programs.nushell.abbreviations.a = "claude"; # `a` short for `agent`

  # `keybindings` and `localPlugins` extend home-manager's own
  # `programs.claude-code` rather than living up here: the plugins that write
  # them (./plugins/*.mod.nix) need home-manager's `config` to resolve the
  # programs they wrap, so the option has to be reachable from that eval.
  modules.home-manager =
    { config, ... }:

    let
      cfgHm = config.programs.claude-code;

      keybindingsByContext = lib.mapAttrsToList (context: bindings: {
        inherit context bindings;
      }) cfgHm.keybindings;

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
              }) cfgHm.localPlugins;
            };
          }
        ]
        ++ lib.concatLists (lib.mapAttrsToList pluginEntries cfgHm.localPlugins)
      );

      # Claude Code needs absolute prefixes; expand a leading `~` to the home dir.
      toAbsolute =
        dir:
        if lib.hasPrefix "~/" dir then "${config.home.homeDirectory}/${lib.removePrefix "~/" dir}" else dir;

      injectDirenvHook = pkgs.callPackage ./hooks/direnv.nix {
        direnv = config.programs.direnv.package;
      };
    in

    {
      options.programs.claude-code = {
        keybindings = lib.mkOption {
          default = { };
          type = lib.types.attrsOf (lib.types.attrsOf (lib.types.nullOr lib.types.str));
          description = ''
            Keybinding overrides for Claude Code, rendered to
            {file}`keybindings.json` inside
            {option}`programs.claude-code.configDir` (default
            {file}`~/.claude/keybindings.json`). Each attribute is a context
            whose value maps keys to commands. The schema field is added
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
            marketplace and enables each plugin via settings.json. Distinct
            from the upstream `plugins` option, which loads external plugin
            directories.
          '';

          type = lib.types.attrsOf (
            lib.types.submodule {
              options = {
                enable = lib.mkOption {
                  type = lib.types.bool;
                  default = false;
                  description = ''
                    Default enablement for this plugin. Every plugin is
                    published to the marketplace and provisioned in
                    settings.json regardless; this only sets the default
                    value, which can be overridden per-project through
                    project-level settings.
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

      config = {
        home = {
          packages = lib.optionals (cfg.voice.package != null) [ cfg.voice.package ];

          # We want the skill directory to bundle SKILL.md alongside the
          # `notify` wrapper, but home-manager's
          # `programs.claude-code.skills.<name>` accepts only `lines` or a Nix
          # `path`. Its `lib.isPath` branch rejects derivation outputs (which
          # are strings), so a `runCommand` that assembles both files can't go
          # through the option. Wire up `home.file` directly instead.
          file = {
            ".claude/skills/notify/SKILL.md".source = ./skills/notify/SKILL.md;
            ".claude/skills/notify/notify".source = lib.getExe notifySkill;
          }
          // lib.optionalAttrs (cfgHm.enable && cfgHm.keybindings != { }) {
            "${cfgHm.configDir}/keybindings.json".source = json.generate "claude-code-keybindings.json" {
              "$schema" = "https://www.schemastore.org/claude-code-keybindings.json";
              bindings = keybindingsByContext;
            };
          };
        };

        programs.claude-code = {
          enable = lib.mkDefault true;
          package = lib.mkDefault pkgs.unstable.custom.claude-code-bin;

          # Shared stuff across all agent tools.
          inherit (agents) rules context commands;
          skills = agents.skills // {
            codex-review = ./skills/codex-review;
          };

          keybindings.Chat = {
            # Default keybind toggles fast mode. Emulator interprets `<esc>o`
            # as `meta+o`, conflicting with vim mode if I'm typing quickly.
            "meta+o" = "chat:newline";
          };

          settings = {
            theme = "dark";
            preferredNotifChannel = "terminal_bell";

            # settings.json is a read-only symlink into the Nix store, so Claude
            # Code's "don't ask again" toggle for auto mode can never persist.
            # Opt out of the confirmation up front.
            skipAutoPermissionPrompt = true;

            # Terrible idea and never should've been added.
            disableDeepLinkRegistration = "disable";

            # Too repetitive.
            spinnerTipsEnabled = false;

            # Offensive.
            autoInstallIdeExtension = false;

            # Combat the doorway effect.
            externalEditorContext = true;

            # Plan iteration munches all my tokens.
            showClearContextOnPlanAccept = true;

            # I don't want uncommitted memory affecting Claude's decisions.
            autoMemoryEnabled = false;

            # Default only seems to render what's in the viewport. Tmux sadness.
            tui = "fullscreen";

            # What could go wrong.
            remoteControlAtStartup = true;

            # I never want these by default.
            disableClaudeAiConnectors = true;

            # I'm not their advertising billboard.
            attribution = {
              commit = "";
              pr = "";
              sessionUrl = false;
            };

            voice = {
              enabled = cfg.voice.package != null;
              mode = "hold";
            };

            worktree = {
              baseRef = "head";
              symlinkDirectories = [
                ".claude/settings.local.json"
                ".direnv"
                ".envrc"
                ".vimrc.lua"
              ];
            };

            permissions = {
              defaultMode = "auto";

              additionalDirectories = map toAbsolute trustedDirectories;
            };

            statusLine = {
              type = "command";
              command = "${statusline}";
            };

            hooks = {
              PreToolUse = [
                (commandHook (lib.getExe blockEnvFilesHook) // { matcher = "Read|Edit|Write"; })
              ];

              PostToolUse = [ (commandHook autoFormatHook // { matcher = "Write|Edit"; }) ];

              Notification = [
                (commandHook (lib.getExe notifyPermissionRequestHook) // { matcher = "permission_prompt"; })
              ];
            }
            # The hook only makes sense when direnv is around to export anything.
            // lib.optionalAttrs config.programs.direnv.enable {
              SessionStart = [ (commandHook injectDirenvHook) ];
            };
          }
          // lib.optionalAttrs (cfgHm.localPlugins != { }) {
            extraKnownMarketplaces.dotfiles.source = {
              source = "directory";
              path = "${marketplace}";
            };

            enabledPlugins = lib.mapAttrs' (
              name: plugin: lib.nameValuePair "${name}@dotfiles" plugin.enable
            ) cfgHm.localPlugins;
          };
        };

        programs.git.ignores = [
          "**/.claude/*.lock"
          "**/.claude/settings.local.json"
          "**/.claude/worktrees"
          "**/CLAUDE.local.md"
        ];
      };
    };
}
