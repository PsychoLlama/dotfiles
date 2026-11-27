{ units, module, ... }:

let
  inherit (units) editor;
in

{
  unit = module.component {
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
