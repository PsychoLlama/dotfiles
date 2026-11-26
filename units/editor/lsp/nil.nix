{ rhizome, units, ... }:

let
  inherit (units) editor;
in

rhizome.unit "editor.lsp.nil" {
  unit = rhizome.component [
    (editor.server {
      name = "nil";
      cmd = pkgs: [ "${pkgs.unstable.nil}/bin/nil" ];
      filetypes = [ "nix" ];
      root_markers = [ "flake.nix" ];
      settings.nil.nix.flake.autoArchive = true;
    })
  ];
}
