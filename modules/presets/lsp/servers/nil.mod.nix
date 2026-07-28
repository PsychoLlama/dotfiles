{
  cfg,
  lib,
  pkgs,
  ...
}:

{
  options.package = lib.mkPackageOption pkgs.unstable "nil" { };

  modules.editor.lsp.servers.nil = {
    cmd = [ "${cfg.package}/bin/nil" ];
    filetypes = [ "nix" ];
    root_markers = [ "flake.nix" ];
    settings.nil.nix.flake.autoArchive = true;
  };
}
