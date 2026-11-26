{ rhizome, units, ... }:

let
  inherit (units.programs) claude-code;
in

rhizome.unit "claude-code.hooks.direnv" {
  unit = rhizome.component [
    (claude-code.settings (
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

      # Wired only when direnv is already installed. Importing the direnv preset
      # from here would make Claude Code the reason it exists; that's a profile's
      # call.
      lib.optionalAttrs config.programs.direnv.enable {

        hooks.SessionStart = [
          {
            hooks = [
              {
                type = "command";
                command = injectDirenv;
              }
            ];
          }
        ];
      }
    ))
  ];
}
