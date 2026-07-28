# The executable half of the `notify` skill: a desktop-notification wrapper
# Claude invokes when it needs attention. `SKILL.md` next door tells it when.

{
  writeShellApplication,
  libnotify,
}:

writeShellApplication {
  name = "notify";
  runtimeInputs = [ libnotify ];
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
}
