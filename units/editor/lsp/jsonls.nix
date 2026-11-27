{ units, module, ... }:

let
  inherit (units) editor;
in

{
  unit = module.component {
    config = [
      (editor.server {
        name = "jsonls";
        cmd = pkgs: [
          "${pkgs.unstable.vscode-langservers-extracted}/bin/vscode-json-language-server"
          "--stdio"
        ];
        root_markers = [ ".git/" ];
        filetypes = [
          "json"
          "jsonc"
          "json5"
        ];
      })
    ];
  };
}
