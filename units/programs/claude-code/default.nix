{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager user;
in

rhizome.unit "claude-code" (claude-code: {
  main = rhizome.component {
    uses = {
      inherit (units.agents) memory;
      trusted-directories = user.trusted-directory;
    };

    config =
      {
        memory,
        trusted-directories,
      }:
      [

        (home-manager.module (
          {
            pkgs,
            lib,
            config,
            ...
          }:

          let

            # Claude Code needs absolute prefixes; expand a leading `~` to the home dir.
            toAbsolute =
              dir:
              if lib.hasPrefix "~/" dir then "${config.home.homeDirectory}/${lib.removePrefix "~/" dir}" else dir;
          in

          {
            config = {
              programs.nushell.abbreviations.a = "claude"; # `a` short for `agent`

              programs.claude-code = {
                enable = lib.mkDefault true;
                package = lib.mkDefault pkgs.unstable.custom.claude-code-bin;

                # Shared stuff across all agent tools.
                inherit (memory) rules context commands;
                skills = lib.mapAttrs' (_: skill: lib.nameValuePair skill.name skill.output.claude) (
                  lib.filterAttrs (_: skill: lib.elem "claude" skill.targets) config.agents.skills
                );

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
              };

              programs.git = {
                ignores = [
                  "**/.claude/*.lock"
                  "**/.claude/settings.local.json"
                  "**/.claude/worktrees"
                  "**/CLAUDE.local.md"
                ];
              };
            };
          }
        ))
      ];
  };

  unit = [
    claude-code.main
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
})
