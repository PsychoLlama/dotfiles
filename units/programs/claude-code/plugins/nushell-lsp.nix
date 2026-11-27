{ units, define, ... }:

let
  inherit (units.programs) claude-code;
in

{
  unit = define.unit {
    provides = [
      (claude-code.plugin ({
        name = "nushell-lsp";
        description = "Nushell Language Server for Claude Code.";

        lsp.nushell = {
          command = "nu";
          args = [ "--lsp" ];
          extensionToLanguage.".nu" = "nu";
        };
      }))
    ];
  };
}
