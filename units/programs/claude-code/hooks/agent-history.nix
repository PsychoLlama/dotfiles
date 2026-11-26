{ rhizome, units, ... }:

# Part of the `agent` nushell command. Saves bash history to a SQLite DB
# that's queried similar to nushell's `history` builtin.

let
  inherit (units.programs) claude-code;
in

rhizome.unit "claude-code.hooks.agent-history" {
  unit = rhizome.component [
    (claude-code.settings (
      {
        config,
        lib,
        pkgs,
        ...
      }:

      let
        nu = lib.getExe config.programs.nushell.package;

        # `--stdin` binds the hook payload to `$in`.
        agentHistory = pkgs.writeScript "agent-history" ''
          #!/usr/bin/env -S ${nu} --stdin --no-config-file
          ${builtins.readFile ./agent-history.nu}
        '';

        hook = [
          {
            matcher = "Bash";
            hooks = [
              {
                type = "command";
                command = "${agentHistory}";
              }
            ];
          }
        ];
      in

      {
        hooks = {
          PostToolUse = hook;
          PostToolUseFailure = hook;
        };
      }
    ))
  ];
}
