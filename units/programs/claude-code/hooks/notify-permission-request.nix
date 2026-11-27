{ units, define, ... }:

let
  inherit (units.programs) claude-code;
in

{
  unit = define.unit {
    provides = [
      (claude-code.settings (
        {
          config,
          lib,
          pkgs,
          ...
        }:

        let
          notifyPermissionRequest = pkgs.writeShellApplication {
            name = "notify-permission-request";
            runtimeInputs = [
              config.programs.jq.package
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
          hooks.Notification = [
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
      ))
    ];
  };
}
