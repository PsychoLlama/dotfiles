{ rhizome, units, ... }:

let
  inherit (units) editor;
in

rhizome.unit "editor.lsp.jsonls" {
  unit = rhizome.component [
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
}
