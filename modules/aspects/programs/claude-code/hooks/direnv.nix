{
  exports.homeManager =
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
      # Wired only when direnv is already installed. Importing the direnv preset
      # from here would make Claude Code the reason it exists; that's a profile's
      # call.
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
    };
}
