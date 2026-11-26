{ rhizome, units, ... }:

let
  inherit (units.programs) claude-code;
in

rhizome.unit "claude-code.plugins.nil-lsp" {
  unit = rhizome.component [
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
}
