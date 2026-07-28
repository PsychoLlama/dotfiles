# A `PreToolUse` hook that refuses reads and writes of `.env` files. Exit
# status 2 is how a hook tells Claude Code the tool call is denied.

{
  writeShellApplication,
  jq,
}:

writeShellApplication {
  name = "block-env-files";
  runtimeInputs = [ jq ];
  text = ''
    file_path=$(jq -r '.tool_input.file_path // ""')
    basename=$(basename "$file_path")

    if [ "$basename" = ".env" ]; then
      echo "Access to .env files is blocked" >&2
      exit 2
    fi
  '';
}
