{ rhizome, units, ... }:

let
  inherit (units.platform) editor;
in

rhizome.component "editor.lsp.rust-analyzer" [
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
]
