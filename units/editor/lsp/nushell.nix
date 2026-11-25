{ rhizome, units, ... }:

let
  inherit (units.platform) editor;
in

rhizome.component "editor.lsp.nushell" [
  (editor.module ({
    lsp.servers.nushell = {
      cmd = [
        "nu"
        "--lsp"
      ];
      filetypes = [ "nu" ];
      root_markers = [ ".git/" ];
    };
  }))
]
