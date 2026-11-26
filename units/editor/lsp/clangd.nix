{ rhizome, units, ... }:

let
  inherit (units) editor;
in

rhizome.unit "editor.lsp.clangd" {
  unit = rhizome.component [
    (editor.server {
      name = "clangd";
      cmd = pkgs: [ "${pkgs.unstable.clang-tools}/bin/clangd" ];
      root_markers = [ "compile_commands.json" ];
      filetypes = [
        "c"
        "cpp"
        "objc"
        "objcpp"
      ];
    })
  ];
}
