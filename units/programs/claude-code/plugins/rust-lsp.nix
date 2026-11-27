{ units, module, ... }:

let
  inherit (units.programs) claude-code;
in

{
  unit = module.component {
    config = [
      (claude-code.plugin ({
        name = "rust-lsp";
        description = "Rust Language Server for Claude Code.";

        lsp.rust = {
          command = "rust-analyzer";
          extensionToLanguage.".rs" = "rust";
        };
      }))
    ];
  };
}
