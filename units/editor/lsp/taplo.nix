{ units, define, ... }:

let
  inherit (units) editor;
in

{
  unit = define.unit {
    config = [
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
  };
}
