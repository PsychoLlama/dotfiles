{ rhizome, units, ... }:

let
  inherit (units.platform) editor;
in

rhizome.unit "editor.lsp.rust-analyzer" {
  unit = rhizome.component [
    (editor.module ({
      lsp.servers.rust-analyzer = {
        cmd = [ "rust-analyzer" ];
        filetypes = [ "rust" ];
        root_markers = [
          "Cargo.lock"
          "Cargo.toml"
        ];
      };
    }))
  ];
}
