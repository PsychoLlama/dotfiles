{
  config,
  lib,
  pkgs,
  ...
}:

let
  direnv = lib.getExe config.programs.direnv.package;

  injectDirenv = pkgs.writers.writeDash "inject-direnv" ''
    [ -z "''${CLAUDE_ENV_FILE:-}" ] && exit 0

    cat >> "''$CLAUDE_ENV_FILE" <<DIRENV
    eval "\$(${direnv} export bash 2>/dev/null)"
    DIRENV
  '';
in

{
  config = lib.mkIf config.programs.direnv.enable {
    programs.claude-code.settings.hooks.SessionStart = [
      {
        hooks = [
          {
            type = "command";
            command = injectDirenv;
          }
        ];
      }
    ];
  };
}
