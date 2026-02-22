{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.claude-code;
in

{
  config = lib.mkIf cfg.enable {
    programs.claude-code.servers.chrome-devtools = {
      # DANGER! Assumes an isolated browser.
      settings = {
        command = "${pkgs.chrome-devtools-mcp}/bin/chrome-devtools-mcp";

        # Only necessary on NixOS. Wish this supported an env variable.
        args = lib.optionals pkgs.stdenv.isLinux [
          "--executablePath"
          "${config.programs.chromium.package}/bin/chromium"
        ];
      };

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
