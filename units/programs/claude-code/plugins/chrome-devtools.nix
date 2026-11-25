{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.component "claude-code.plugins.chrome-devtools" [
  (home-manager.module (
    {
      config,
      lib,
      pkgs,
      ...
    }:

    let
      chromiumExe = lib.getExe' config.programs.chromium.package "chromium";

      wrapper = pkgs.writeShellApplication {
        name = "chrome-devtools-mcp";
        text = ''
          if [ "''${CHROME_MCP_AUTOCONNECT:-}" = "true" ]; then
            args=(--autoConnect ${lib.optionalString pkgs.stdenv.isLinux "--userDataDir ${config.home.homeDirectory}/.config/chromium"})
          else
            args=(${lib.optionalString pkgs.stdenv.isLinux "--executablePath ${chromiumExe}"})
          fi

          exec ${lib.getExe pkgs.custom.chrome-devtools-mcp} "''${args[@]}" "$@"
        '';
      };
    in

    {
      programs.claude-code.plugins.chrome-devtools = {
        description = "Chrome DevTools MCP server for Claude Code.";
        mcp.servers.chrome-devtools.command = lib.getExe wrapper;
      };
    }
  ))
]
