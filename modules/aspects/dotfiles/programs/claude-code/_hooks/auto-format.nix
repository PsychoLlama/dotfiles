{ lib, pkgs, ... }:

let
  jq = lib.getExe pkgs.jq;

  autoFormat =
    pkgs.writers.writeDash "auto-format"
      # sh
      ''
        command -v treefmt >/dev/null 2>&1 || exit 0

        file_path=$(${jq} -r '.tool_input.file_path // ""')

        [ -z "$file_path" ] || [ ! -f "$file_path" ] && exit 0

        treefmt "$file_path" 2>/dev/null || true
      '';
in

{
  programs.claude-code.settings.hooks.PostToolUse = [
    {
      matcher = "Write|Edit";
      hooks = [
        {
          type = "command";
          command = autoFormat;
        }
      ];
    }
  ];
}
