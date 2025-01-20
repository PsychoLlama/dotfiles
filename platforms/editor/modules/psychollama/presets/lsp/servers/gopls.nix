{
  lib,
  config,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.lsp.servers.gopls;
in

{
  options.psychollama.presets.lsp.servers.gopls = {
    enable = lib.mkEnableOption "Use the `gopls` language server";
    package = lib.mkPackageOption pkgs.unstable "gopls" { };
  };

  config.lsp.servers.gopls = lib.mkIf cfg.enable {
    server = "${pkgs.unstable.gopls}/bin/gopls";
    args = [ "-remote=auto" ];
    filetypes = [ "go" ];
    root.patterns = [ "go.mod" ];
  };
}
