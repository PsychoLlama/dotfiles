{ units, define, ... }:

let
  inherit (units.programs) claude-code;
in

{
  unit = define.unit {
    provides = [
      (claude-code.plugin (
        {
          lib,
          config,
          pkgs,
          ...
        }:

        {

          name = "nil-lsp";
          description = "Nil (nix) Language Server for Claude Code.";

          lsp.nil = {
            command = lib.getExe pkgs.unstable.nil;
            extensionToLanguage.".nix" = "nix";
          };
        }
      ))
    ];
  };
}
