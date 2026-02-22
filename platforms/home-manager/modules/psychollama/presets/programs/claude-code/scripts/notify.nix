{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.claude-code;

  notify = pkgs.writeShellApplication {
    name = "notify";
    runtimeInputs = [ pkgs.libnotify ];
    text = ''
      title="Claude Code"
      icon="dialog-information"

      while [ $# -gt 0 ]; do
        case "$1" in
          --title) title="$2"; shift 2 ;;
          --icon) icon="$2"; shift 2 ;;
          *) break ;;
        esac
      done

      if [ $# -eq 0 ]; then
        echo "Usage: notify [--title TITLE] [--icon ICON] <message>" >&2
        exit 1
      fi

      message="$*"

      case "$(uname)" in
        Darwin)
          osascript -e "display notification \"$message\" with title \"$title\""
          ;;
        *)
          notify-send \
            --urgency=normal \
            --icon="$icon" \
            "$title" \
            "$message"
          ;;
      esac
    '';
  };
in

{
  config = lib.mkIf cfg.enable {
    programs.claude-code.scripts.notify = {
      source = lib.getExe notify;
      allow = true;
    };
  };
}
