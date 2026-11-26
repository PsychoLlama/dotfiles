{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.unit "claude-code.plugins.rust-lsp" {
  unit = rhizome.component [
    (home-manager.module ({
      programs.claude-code.plugins.rust-lsp = {
        description = "Rust Language Server for Claude Code.";

        lsp.servers.rust = {
          command = "rust-analyzer";
          extensionToLanguage.".rs" = "rust";
        };
      };
    }))
  ];
}
