{ rhizome, units, ... }:

let
  inherit (units) editor;
in

rhizome.unit "editor.lsp.typescript" {
  unit = rhizome.component [
    (editor.server {
      name = "typescript";
      cmd = pkgs: [
        "${pkgs.unstable.typescript-language-server}/bin/typescript-language-server"
        "--stdio"
      ];
      filetypes = [
        "typescript"
        "typescriptreact"
        "javascript"
        "javascriptreact"
      ];

      root_markers = [
        "tsconfig.json"
        ".git/"
      ];
    })

    (editor.tool (pkgs: pkgs.unstable.unzip))
    (editor.tool (pkgs: pkgs.unstable.yarn))
  ];
}
