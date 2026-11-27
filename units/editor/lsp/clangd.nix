{ units, define, ... }:

let
  inherit (units) editor;
in

{
  unit = define.unit {
    config = [
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
  };
}
