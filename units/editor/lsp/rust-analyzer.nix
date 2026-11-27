{ units, define, ... }:

let
  inherit (units) editor;
in

{
  unit = define.unit {
    config = [
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
  };
}
