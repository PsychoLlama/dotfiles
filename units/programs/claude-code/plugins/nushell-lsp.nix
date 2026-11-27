{ units, module, ... }:

let
  inherit (units.programs) claude-code;
in

{
  unit = module.component "unit" {
    config = [
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
