{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.claude-code;
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
  config = lib.mkIf cfg.enable {
    programs.claude-code.servers.chrome-devtools = {
      settings.command = lib.getExe wrapper;

      permissions.allow = [
        "click"
        "close_page"
        "drag"
        "emulate"
        "evaluate_script"
        "fill"
        "fill_form"
        "get_console_message"
        "get_network_request"
        "handle_dialog"
        "hover"
        "list_console_messages"
        "list_network_requests"
        "list_pages"
        "navigate_page"
        "new_page"
        "performance_analyze_insight"
        "performance_start_trace"
        "performance_stop_trace"
        "press_key"
        "resize_page"
        "select_page"
        "take_screenshot"
        "take_snapshot"
        "upload_file"
        "wait_for"
      ];
    };
  };
}
