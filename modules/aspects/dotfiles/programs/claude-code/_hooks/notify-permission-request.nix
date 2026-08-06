{ lib, pkgs, ... }:

let
  notifyPermissionRequest = pkgs.writeShellApplication {
    name = "notify-permission-request";
    runtimeInputs = [
      pkgs.jq
      pkgs.libnotify
    ];
    text = ''
      input=$(cat)
      message=$(echo "$input" | jq -r '.message // "Permission requested"')
      cwd=$(echo "$input" | jq -r '.cwd // ""')
      project=$(basename "$cwd")

      if [ -n "$project" ]; then
        title="Claude Code ($project)"
      else
        title="Claude Code"
      fi

      notify-send --urgency=normal --icon=dialog-question "$title" "$message"
    '';
  };
in

{
  programs.claude-code.settings.hooks.Notification = [
    {
      matcher = "permission_prompt";
      hooks = [
        {
          type = "command";
          command = lib.getExe notifyPermissionRequest;
        }
      ];
    }
  ];
}
