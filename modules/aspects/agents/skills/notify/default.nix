{
  exports.homeManager =
    { pkgs, lib, ... }:

    let
      notify = pkgs.writeShellApplication {
        name = "notify";
        runtimeInputs = [ pkgs.libnotify ];
        text = ''
          title="Agent"
          if [ -n "''${CLAUDE_CODE_SESSION_ID:-}" ]; then
            title="Claude Code"
          elif [ -n "''${CODEX_SESSION_ID:-}" ]; then
            title="Codex"
          fi

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
    in

    {
      agents.skills.notify = {
        files.notify = lib.getExe notify;
        template.body = ./SKILL.md;
        description = "Send a desktop notification to get the user's attention. Use after a long-running task finishes, or when you need a response and the user is likely away from the terminal.";
      };
    };
}
