{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.component "claude-code.plugins.rust-lsp" [
  (home-manager.module ({
    programs.claude-code.plugins.rust-lsp = {
      description = "Rust Language Server for Claude Code.";

      lsp.servers.rust = {
        command = "rust-analyzer";
        extensionToLanguage.".rs" = "rust";
      };
    };
  }))
]
