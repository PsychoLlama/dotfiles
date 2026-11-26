{ rhizome, units, ... }:

let
  inherit (units.platform) editor;
in

rhizome.unit "editor.lsp.nushell" {
  unit = rhizome.component [
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
  ];
}
