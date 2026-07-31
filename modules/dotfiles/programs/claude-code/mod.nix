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

  # Claude Code addresses a plugin as `<plugin>@<marketplace>`, so the
  # marketplace name is part of the settings schema, not just a label.
  marketplaceName = "dotfiles";

  # Where Claude Code looks, relative to the home directory. Fixed: the CLI
  # only reads elsewhere when `CLAUDE_CONFIG_DIR` says so, and nothing here
  # wants to move it.
  configDir = ".claude";

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

  # The marketplace is a directory of plugin directories plus a manifest
  # naming them. Claude Code reads it straight out of the store.
  marketplace = pkgs.linkFarm "claude-marketplace-${marketplaceName}" (
    [
      {
        name = ".claude-plugin/marketplace.json";
        path = json.generate "marketplace.json" {
          name = marketplaceName;
          owner.name = marketplaceName;
          plugins = lib.mapAttrsToList (name: plugin: {
            inherit name;
            inherit (plugin) description;
            source = "./plugins/${name}";
          }) cfg.marketplace.plugins;
        };
      }
    ]
    ++ lib.concatLists (lib.mapAttrsToList pluginEntries cfg.marketplace.plugins)
  );

  # The `notify` skill is a directory: prose for Claude and the wrapper the
  # prose tells it to run.
  notifySkillDir = pkgs.linkFarm "claude-skill-notify" [
    {
      name = "SKILL.md";
      path = ./skills/notify/SKILL.md;
    }
    {
      name = "notify";
      path = lib.getExe notifySkill;
    }
  ];
in

{
  options = {
    package = lib.mkOption {
      type = lib.types.package;
      default = pkgs.unstable.custom.claude-code-bin;
      defaultText = lib.literalExpression "pkgs.unstable.custom.claude-code-bin";
      description = "The Claude Code package to install.";
    };

    voice.package = lib.mkPackageOption pkgs.unstable "sox" {
      nullable = true;
    };

    settings = lib.mkOption {
      type = json.type;
      default = { };
      description = ''
        Contents of {file}`settings.json`. The schema field is added
        automatically, as are the settings that can only be resolved against
        home-manager (absolute trusted directories, the direnv hook).
      '';
    };

    context = lib.mkOption {
      type = lib.types.lines;
      default = "";
      description = ''
        Global memory, rendered to {file}`CLAUDE.md` inside
        {file}`~/.claude`.
      '';
    };

    rules = lib.mkOption {
      type = lib.types.attrsOf lib.types.path;
      default = { };
      description = ''
        Path-scoped rule files, keyed by rule name. Each is rendered to
        {file}`rules/<name>.md` inside {file}`~/.claude`.
      '';
    };

    commands = lib.mkOption {
      type = lib.types.attrsOf lib.types.path;
      default = { };
      description = ''
        Slash-command prompts, keyed by command name. Each is rendered to
        {file}`commands/<name>.md` inside {file}`~/.claude`.
      '';
    };

    skills = lib.mkOption {
      type = lib.types.attrsOf lib.types.path;
      default = { };
      description = ''
        Skill directories, keyed by skill name. Each holds a `SKILL.md` (plus
        whatever else it needs) and is linked into {file}`skills/<name>`
        inside {file}`~/.claude`. Store paths are fine, so a skill may be
        assembled by a derivation.
      '';
    };

    keybindings = lib.mkOption {
      default = { };
      type = lib.types.attrsOf (lib.types.attrsOf (lib.types.nullOr lib.types.str));
      description = ''
        Keybinding overrides, rendered to {file}`keybindings.json` inside
        {file}`~/.claude`. Each attribute is a context whose value maps keys
        to commands. The schema field is added automatically. Bind a key to
        `null` to unbind it.
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

    marketplace.plugins = lib.mkOption {
      default = { };
      description = ''
        Plugins published through this repo's own marketplace. Each becomes a
        plugin directory in the generated marketplace, provisioned and
        available to Claude Code whether or not it is activated.

        The presets under {file}`plugins/` are the ones this repo ships. They
        contribute an entry here when enabled, so a disabled preset is not
        published at all.
      '';

      type = lib.types.attrsOf (
        lib.types.submodule {
          options = {
            activate = lib.mkOption {
              type = lib.types.bool;
              default = false;
              description = ''
                Whether Claude Code loads this plugin without being asked.
                Provisioning is not the question — the plugin is published to
                the marketplace either way. This is the default it gets in
                {file}`settings.json`, which project-level settings override.
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

  config.programs = {
    # The plugins are claude-code's own — they configure it and mean nothing
    # without it. Enabling them from here keeps them on by default while
    # leaving each one individually overridable.
    claude-code = {
      plugins = {
        chrome-devtools.enable = lib.mkDefault true;
        lua-lsp.enable = lib.mkDefault true;
        nil-lsp.enable = lib.mkDefault true;
        nushell-lsp.enable = lib.mkDefault true;
        rust-lsp.enable = lib.mkDefault true;
        typescript-lsp.enable = lib.mkDefault true;
      };

      # Shared stuff across all agent tools.
      inherit (agents) context rules commands;

      skills = agents.skills // {
        codex-review = ./skills/codex-review;
        notify = notifySkillDir;
      };

      keybindings.Chat = {
        # Default keybind toggles fast mode. Emulator interprets `<esc>o`
        # as `meta+o`, conflicting with vim mode if I'm typing quickly.
        "meta+o" = "chat:newline";
      };

      settings = lib.mkMerge [
        {
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

          permissions.defaultMode = "auto";

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
          };
        }

        (lib.mkIf (cfg.marketplace.plugins != { }) {
          extraKnownMarketplaces.${marketplaceName}.source = {
            source = "directory";
            path = "${marketplace}";
          };

          enabledPlugins = lib.mapAttrs' (
            name: plugin: lib.nameValuePair "${name}@${marketplaceName}" plugin.activate
          ) cfg.marketplace.plugins;
        })
      ];
    };

    nushell.abbreviations.a = "claude"; # `a` short for `agent`
  };

  modules.home-manager =
    { config, ... }:

    let
      # Claude Code needs absolute prefixes; expand a leading `~` to the home dir.
      toAbsolute =
        dir:
        if lib.hasPrefix "~/" dir then "${config.home.homeDirectory}/${lib.removePrefix "~/" dir}" else dir;

      injectDirenvHook = pkgs.callPackage ./hooks/direnv.nix {
        direnv = config.programs.direnv.package;
      };

      # The settings that can only be resolved down here, against
      # home-manager's fixpoint. Everything portable is in `cfg.settings`.
      # This is a plain value on its way into a derivation, not an option
      # definition, so it merges by hand rather than through `mkMerge`.
      settings = lib.recursiveUpdate cfg.settings {
        "$schema" = "https://json.schemastore.org/claude-code-settings.json";

        permissions.additionalDirectories = map toAbsolute trustedDirectories;

        # The hook only makes sense when direnv is around to export anything.
        hooks = lib.optionalAttrs config.programs.direnv.enable {
          SessionStart = [ (commandHook injectDirenvHook) ];
        };
      };

      markdownFiles =
        subdir:
        lib.mapAttrs' (
          name: source: lib.nameValuePair "${configDir}/${subdir}/${name}.md" { inherit source; }
        );

      skillDirs = lib.mapAttrs' (
        name: source:
        lib.nameValuePair "${configDir}/skills/${name}" {
          inherit source;
          recursive = true;
        }
      ) cfg.skills;
    in

    {
      home = {
        packages = [ cfg.package ] ++ lib.optional (cfg.voice.package != null) cfg.voice.package;

        file = lib.mkMerge [
          {
            "${configDir}/settings.json".source = json.generate "claude-code-settings.json" settings;
          }

          (lib.mkIf (cfg.context != "") {
            "${configDir}/CLAUDE.md".text = cfg.context;
          })

          (lib.mkIf (cfg.keybindings != { }) {
            "${configDir}/keybindings.json".source = json.generate "claude-code-keybindings.json" {
              "$schema" = "https://www.schemastore.org/claude-code-keybindings.json";
              bindings = lib.mapAttrsToList (context: bindings: {
                inherit context bindings;
              }) cfg.keybindings;
            };
          })

          (markdownFiles "rules" cfg.rules)
          (markdownFiles "commands" cfg.commands)
          skillDirs
        ];
      };

      programs.git.ignores = [
        "**/.claude/*.lock"
        "**/.claude/settings.local.json"
        "**/.claude/worktrees"
        "**/CLAUDE.local.md"
      ];
    };
}
