{ rhizome, units, ... }:

let
  inherit (units) editor;
in

rhizome.unit "editor.lsp.nushell" {
  unit = rhizome.component [
    (editor.server {
      name = "nushell";
      cmd = [
        "nu"
        "--lsp"
      ];
      filetypes = [ "nu" ];
      root_markers = [ ".git/" ];
    })
  ];
}
