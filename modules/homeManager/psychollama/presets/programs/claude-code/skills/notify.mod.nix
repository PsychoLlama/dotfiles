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
in

{
  # We want the skill directory to bundle SKILL.md alongside the `notify`
  # wrapper, but home-manager's `programs.claude-code.skills.<name>` accepts
  # only `lines` or a Nix `path`. Its `lib.isPath` branch rejects derivation
  # outputs (which are strings), so a `runCommand` that assembles both files
  # can't go through the option. Wire up `home.file` directly instead.
  config = lib.mkIf cfg.enable {
    home.file = {
      ".claude/skills/notify/SKILL.md".source = ./notify/SKILL.md;
      ".claude/skills/notify/notify".source = lib.getExe notify;
    };
  };
}
