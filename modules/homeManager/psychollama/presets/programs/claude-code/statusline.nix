{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.claude-code;

  nu = lib.getExe pkgs.nushell;

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
  config = lib.mkIf cfg.enable {
    programs.claude-code.settings.statusLine = {
      type = "command";
      command = "${statusline}";
    };
  };
}
