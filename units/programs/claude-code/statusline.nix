{ rhizome, units, ... }:

let
  inherit (units.programs) claude-code;
in

rhizome.unit "claude-code.statusline" {
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

        # Own the shebang so we can pass `--stdin` (bind stdin to `$in`, required
        # because Claude Code's stdin is a socket) alongside `--no-config-file`.
        # `env -S` splits the arguments — a bare `#!<nu> --stdin --no-config-file`
        # would reach nushell as a single malformed argument.
        statusline = pkgs.writeScript "statusline" ''
          #!/usr/bin/env -S ${nu} --stdin --no-config-file
          ${builtins.readFile ./statusline.nu}
        '';
      in

      {
        statusLine = {
          type = "command";
          command = "${statusline}";
        };
      }
    ))
  ];
}
