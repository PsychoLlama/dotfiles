{ units, module, ... }:

let
  inherit (units) editor;
in

{
  unit = module.component {
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
