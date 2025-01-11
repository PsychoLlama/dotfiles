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
    enable = lib.mkEnableOption "Use Nil (nix) language server";
    package = lib.mkPackageOption pkgs.unstable "nil" { };
  };

  config.lsp.servers.nil = lib.mkIf cfg.enable {
    server = "${cfg.package}/bin/nil";
    filetypes = [ "nix" ];
    root.patterns = [ "flake.nix" ];
    settings.nil.nix.flake.autoArchive = true;
  };
}
