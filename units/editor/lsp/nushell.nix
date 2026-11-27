{ units, define, ... }:

let
  inherit (units) editor;
in

{
  unit = define.unit {
    config = [
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
  };
}
