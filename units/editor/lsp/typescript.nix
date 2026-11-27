{ units, module, ... }:

let
  inherit (units) editor;
in

{
  unit = module.component "unit" {
    config = [
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
  };
}
