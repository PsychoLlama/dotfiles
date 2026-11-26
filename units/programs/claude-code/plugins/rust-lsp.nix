{ rhizome, units, ... }:

let
  inherit (units.programs) claude-code;
in

rhizome.unit "claude-code.plugins.rust-lsp" {
  unit = rhizome.component [
    (claude-code.plugin ({
      name = "rust-lsp";
      description = "Rust Language Server for Claude Code.";

      lsp.rust = {
        command = "rust-analyzer";
        extensionToLanguage.".rs" = "rust";
      };
    }))
  ];
}
