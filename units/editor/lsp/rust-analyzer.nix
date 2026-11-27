{ units, module, ... }:

let
  inherit (units) editor;
in

{
  unit = module.component "unit" {
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
