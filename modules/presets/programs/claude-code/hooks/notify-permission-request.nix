# A `Notification` hook that raises a desktop notification when Claude asks for
# permission, so a prompt sitting in a backgrounded terminal still gets noticed.
# The project directory goes in the title to disambiguate parallel sessions.

{
  writeShellApplication,
  jq,
  libnotify,
}:

writeShellApplication {
  name = "notify-permission-request";
  runtimeInputs = [
    jq
    libnotify
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
}
