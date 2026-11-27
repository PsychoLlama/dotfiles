{
  rhizome,
  units,
  self,
  module,
  ...
}:

let
  t = rhizome.types;
  inherit (units) agents;
  inherit (units.platform) home-manager user;

  # Everything written here may hold packages, so it may be a function of
  # the Home Manager module's arguments: `{ config, lib, pkgs, ... }: { }`.
  written =
    name: type:
    module.channel name {
      level = user.level;
      type = t.either type t.function;
      merge = rhizome.merge.list;
      optional = true;
    };

  plugin = t.from {
    name = t.str;
    description = t.default t.str "";

    # LSP and MCP servers by name, rendered to `.lsp.json` and `.mcp.json`
    # at the plugin root.
    lsp = t.default t.attrs { };
    mcp = t.default t.attrs { };
  };
in

{
  # Part of `settings.json`. Attrsets merge and lists join.
  settings = written "settings" t.attrs;

  # A locally-defined plugin, published through an inline marketplace.
  #
  # Every plugin is provisioned *disabled*: the marketplace makes it
  # available, and each repository enables the ones it wants through its own
  # project settings.
  plugin = written "plugin" plugin;

  # Replaces home-manager's `programs.claude-code`. We use almost none of it
  # and fight the rest: its plugin support wraps the binary (which breaks
  # Claude's CLI flag parsing outright), its `marketplaces` option overwrites
  # `settings.extraKnownMarketplaces` wholesale, and its `skills` rejects
  # derivations. What's left is a thin mapping onto `home.file`.
  main = module.component "main" {
    uses = {
      inherit (agents) memory;
      skills = agents.skill;
      trusted-directories = user.trusted-directory;
      settings = self.settings;
      plugins = self.plugin;
    };

    config =
      {
        memory,
        skills,
        trusted-directories,
        settings,
        plugins,
      }:
      [
        # `a` short for `agent`
        (units.programs.nushell.abbreviation { a = "claude"; })

        (home-manager.module (
          args@{
            pkgs,
            lib,
            config,
            ...
          }:

          let
            json = pkgs.formats.json { };
            root = "${config.home.homeDirectory}/.claude";

            resolve = value: if lib.isFunction value then value args else value;

            # Attrsets merge, lists join, and anything else takes the last.
            merge = lib.zipAttrsWith (
              _: values:
              if lib.all lib.isAttrs values then
                merge values
              else if lib.all lib.isList values then
                lib.concatLists values
              else
                lib.last values
            );

            # Claude Code needs absolute prefixes; expand a leading `~` to the home dir.
            toAbsolute =
              dir:
              if lib.hasPrefix "~/" dir then "${config.home.homeDirectory}/${lib.removePrefix "~/" dir}" else dir;

            installed = lib.sortOn (plugin: plugin.name) (
              map (value: t.validate "claude-code.plugin" plugin (resolve value)) plugins
            );

            manifests =
              plugin:
              [
                {
                  name = "plugins/${plugin.name}/.claude-plugin/plugin.json";
                  path = json.generate "plugin.json" { inherit (plugin) name description; };
                }
              ]
              ++ lib.optional (plugin.lsp != { }) {
                name = "plugins/${plugin.name}/.lsp.json";
                path = json.generate "lsp.json" plugin.lsp;
              }
              ++ lib.optional (plugin.mcp != { }) {
                name = "plugins/${plugin.name}/.mcp.json";
                path = json.generate "mcp.json" plugin.mcp;
              };

            # Copied, not linked. Claude refuses manifests that resolve outside the
            # plugin directory.
            marketplace = pkgs.runCommand "claude-marketplace" { } (
              lib.concatMapStrings
                (entry: ''
                  install -Dm644 ${entry.path} "$out/${entry.name}"
                '')
                (
                  [
                    {
                      name = ".claude-plugin/marketplace.json";
                      path = json.generate "marketplace.json" {
                        name = "managed";
                        owner.name = "Nix";
                        plugins = map (plugin: {
                          inherit (plugin) name description;
                          source = "./plugins/${plugin.name}";
                        }) installed;
                      };
                    }
                  ]
                  ++ lib.concatMap manifests installed
                )
            );

            # `<root>/<subdir>/<name>.md` for each entry.
            markdown =
              subdir:
              lib.mapAttrs' (name: source: lib.nameValuePair "${root}/${subdir}/${name}.md" { inherit source; });

            # Swap `ctrl+o` (transcript) and `ctrl+g` (editor).
            keybindings = {
              # Global bindings take precedence over Chat.
              Global = {
                "ctrl+o" = "chat:externalEditor";
                "ctrl+g" = "app:toggleTranscript";
              };

              Chat = {
                # Default keybind toggles fast mode. Emulator interprets `<esc>o`
                # as `meta+o`, conflicting with vim mode if I'm typing quickly.
                "meta+o" = "chat:newline";
                "ctrl+g" = null;
                "ctrl+o" = "chat:externalEditor";
              };
            };

            own = {
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

              # Default style narrates too much.
              outputStyle = "Concise";

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

                additionalDirectories = map toAbsolute trusted-directories;
              };
            };
          in

          {
            # Deliberately unwrapped. Wrapping `claude` breaks its CLI flag parsing.
            home.packages = [ pkgs.unstable.custom.claude-code-bin ];

            home.file = lib.mkMerge [
              {
                "${root}/CLAUDE.md".text = memory.context;

                "${root}/settings.json".source = json.generate "claude-code-settings.json" (
                  merge (
                    [ own ]
                    ++ map resolve settings
                    ++ lib.optional (installed != [ ]) {
                      # Declared here rather than in `plugins/known_marketplaces.json`, which
                      # Claude Code rewrites at runtime and cannot be a read-only store symlink.
                      extraKnownMarketplaces.managed.source = {
                        source = "directory";
                        path = "${marketplace}";
                      };

                      enabledPlugins = lib.listToAttrs (
                        map (plugin: lib.nameValuePair "${plugin.name}@managed" false) installed
                      );
                    }
                    ++ [ { "$schema" = "https://json.schemastore.org/claude-code-settings.json"; } ]
                  )
                );

                "${root}/keybindings.json".source = json.generate "claude-code-keybindings.json" {
                  "$schema" = "https://www.schemastore.org/claude-code-keybindings.json";
                  bindings = lib.mapAttrsToList (context: bindings: { inherit context bindings; }) keybindings;
                };
              }

              (markdown "rules" memory.rules)
              (markdown "commands" memory.commands)

              # `recursive` links each file inside the skill separately, so
              # `skills/<name>` stays a real directory. Without it the whole thing is
              # one symlink into the store, and nothing can be dropped alongside a
              # skill at runtime.
              (lib.mapAttrs' (
                name: source:
                lib.nameValuePair "${root}/skills/${name}" {
                  inherit source;
                  recursive = true;
                }
              ) (agents.lib.skills pkgs "claude" skills))
            ];

            programs.git.ignores = [
              "**/.claude/*.lock"
              "**/.claude/settings.local.json"
              "**/.claude/worktrees"
              "**/CLAUDE.local.md"
            ];
          }
        ))
      ];
  };

  unit = module.group "unit" {
    config = [
      self.main
      units.programs.claude-code.hooks.agent-history.unit
      units.programs.claude-code.hooks.auto-format.unit
      units.programs.claude-code.hooks.block-env-files.unit
      units.programs.claude-code.hooks.direnv.unit
      units.programs.claude-code.hooks.notify-permission-request.unit
      units.programs.claude-code.plugins.chrome-devtools.unit
      units.programs.claude-code.plugins.lua-lsp.unit
      units.programs.claude-code.plugins.nil-lsp.unit
      units.programs.claude-code.plugins.nushell-lsp.unit
      units.programs.claude-code.plugins.rust-lsp.unit
      units.programs.claude-code.plugins.typescript-lsp.unit
      units.programs.claude-code.statusline.unit
    ];
  };
}
