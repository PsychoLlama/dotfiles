{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.component "claude-code.plugins.nushell-lsp" [
  (home-manager.module ({
    programs.claude-code.plugins.nushell-lsp = {
      description = "Nushell Language Server for Claude Code.";

      lsp.servers.nushell = {
        command = "nu";
        args = [ "--lsp" ];
        extensionToLanguage.".nu" = "nu";
      };
    };
  }))
]
