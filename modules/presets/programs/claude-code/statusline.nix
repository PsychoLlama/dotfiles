{
  lib,
  writeScript,
  nushell,
}:

let
  nu = lib.getExe nushell;
in

# Own the shebang so we can pass `--stdin` (bind stdin to `$in`, required
# because Claude Code's stdin is a socket) alongside `--no-config-file`.
# `env -S` splits the arguments — a bare `#!<nu> --stdin --no-config-file`
# would reach nushell as a single malformed argument.
writeScript "statusline" ''
  #!/usr/bin/env -S ${nu} --stdin --no-config-file
  ${builtins.readFile ./statusline.nu}
''
