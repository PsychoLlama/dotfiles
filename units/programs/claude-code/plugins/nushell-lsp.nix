{ rhizome, units, ... }:

let
  inherit (units.programs) claude-code;
in

rhizome.unit "claude-code.plugins.nushell-lsp" {
  unit = rhizome.component [
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
}
