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
      notify-send --urgency=normal --icon="$icon" "$title" "$message"
    '';
  };

  # SKILL.md tells the model to run `$CLAUDE_SKILL_DIR/notify`, so the wrapper
  # has to sit beside it in the skill directory.
  notifySkill = pkgs.linkFarm "claude-skill-notify" [
    {
      name = "SKILL.md";
      path = ./notify/SKILL.md;
    }
    {
      name = "notify";
      path = lib.getExe notify;
    }
  ];
in

{
  config = lib.mkIf cfg.enable {
    programs.claude-code.skills.notify = notifySkill;
  };
}
