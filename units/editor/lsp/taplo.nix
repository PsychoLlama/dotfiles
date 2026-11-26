{ rhizome, units, ... }:

let
  inherit (units) editor;
in

rhizome.unit "editor.lsp.taplo" {
  unit = rhizome.component [
    (editor.server {
      name = "taplo";
      cmd = pkgs: [
        "${pkgs.unstable.taplo}/bin/taplo"
        "lsp"
        "stdio"
      ];
      filetypes = [ "toml" ];
      root_markers = [
        ".taplo.toml"
        "taplo.toml"
        ".git/"
      ];
    })
  ];
}
