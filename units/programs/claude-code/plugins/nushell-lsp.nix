{ rhizome, units, ... }:

let
  inherit (units.platform) home-manager;
in

rhizome.unit "claude-code.plugins.nushell-lsp" {
  unit = rhizome.component [
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
  ];
}
