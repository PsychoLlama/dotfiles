{ units, module, ... }:

let
  inherit (units) editor;
in

{
  unit = module.component "unit" {
    config = [
      (editor.server {
        name = "nil";
        cmd = pkgs: [ "${pkgs.unstable.nil}/bin/nil" ];
        filetypes = [ "nix" ];
        root_markers = [ "flake.nix" ];
        settings.nil.nix.flake.autoArchive = true;
      })
    ];
  };
}
