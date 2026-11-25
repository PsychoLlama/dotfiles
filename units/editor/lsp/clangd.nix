{ rhizome, units, ... }:

let
  inherit (units.platform) editor;
in

rhizome.component "editor.lsp.clangd" [
  (editor.module (
    {
      lib,
      config,
      pkgs,
      ...
    }:

    let
      cfg = config.psychollama.presets.lsp.servers.clangd;
    in

    {
      options.psychollama.presets.lsp.servers.clangd = {
        package = lib.mkPackageOption pkgs.unstable "clang-tools" { };
      };

      config.lsp.servers.clangd = {
        cmd = [ "${cfg.package}/bin/clangd" ];
        root_markers = [ "compile_commands.json" ];
        filetypes = [
          "c"
          "cpp"
          "objc"
          "objcpp"
        ];
      };
    }
  ))
]
