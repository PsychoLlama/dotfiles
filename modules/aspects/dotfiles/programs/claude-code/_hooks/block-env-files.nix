{ lib, pkgs, ... }:

let
  blockEnvFiles = pkgs.writeShellApplication {
    name = "block-env-files";
    runtimeInputs = [ pkgs.jq ];
    text = ''
      file_path=$(jq -r '.tool_input.file_path // ""')
      basename=$(basename "$file_path")

      if [ "$basename" = ".env" ]; then
        echo "Access to .env files is blocked" >&2
        exit 2
      fi
    '';
  };
in

{
  programs.claude-code.settings.hooks.PreToolUse = [
    {
      matcher = "Read|Edit|Write";
      hooks = [
        {
          type = "command";
          command = lib.getExe blockEnvFiles;
        }
      ];
    }
  ];
}
