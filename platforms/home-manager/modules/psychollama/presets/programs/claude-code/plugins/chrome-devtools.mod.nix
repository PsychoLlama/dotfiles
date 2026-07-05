{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.claude-code.plugins.chrome-devtools;
  chromiumExe = lib.getExe' config.programs.chromium.package "chromium";

  wrapper = pkgs.writeShellApplication {
    name = "chrome-devtools-mcp";
    text = ''
      if [ "''${CHROME_MCP_AUTOCONNECT:-}" = "true" ]; then
        args=(--autoConnect ${lib.optionalString pkgs.stdenv.isLinux "--userDataDir ${config.home.homeDirectory}/.config/chromium"})
      else
        args=(${lib.optionalString pkgs.stdenv.isLinux "--executablePath ${chromiumExe}"})
      fi

      exec ${lib.getExe pkgs.chrome-devtools-mcp} "''${args[@]}" "$@"
    '';
  };
in

{
  options.psychollama.presets.programs.claude-code.plugins.chrome-devtools = {
    enable = lib.mkEnableOption "Chrome DevTools MCP for Claude Code" // {
      default = true;
    };
  };

  config = lib.mkIf cfg.enable {
    programs.claude-code.localPlugins.chrome-devtools = {
      description = "Chrome DevTools MCP server for Claude Code.";
      mcp.servers.chrome-devtools.command = lib.getExe wrapper;
    };
  };
}
