{ rhizome, units, ... }:

let
  inherit (units.platform) editor;
in

rhizome.unit "editor.lsp.nil" {
  unit = rhizome.component [
    (editor.module (
      {
        lib,
        config,
        pkgs,
        ...
      }:

      let
        cfg = config.psychollama.presets.lsp.servers.nil;
      in

      {
        options.psychollama.presets.lsp.servers.nil = {
          package = lib.mkPackageOption pkgs.unstable "nil" { };
        };

        config.lsp.servers.nil = {
          cmd = [ "${cfg.package}/bin/nil" ];
          filetypes = [ "nix" ];
          root_markers = [ "flake.nix" ];
          settings.nil.nix.flake.autoArchive = true;
        };
      }
    ))
  ];
}
