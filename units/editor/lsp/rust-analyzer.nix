{ rhizome, units, ... }:

let
  inherit (units) editor;
in

rhizome.unit "editor.lsp.rust-analyzer" {
  unit = rhizome.component [
    (editor.server {
      name = "rust-analyzer";
      cmd = [ "rust-analyzer" ];
      filetypes = [ "rust" ];
      root_markers = [
        "Cargo.lock"
        "Cargo.toml"
      ];
    })
  ];
}
