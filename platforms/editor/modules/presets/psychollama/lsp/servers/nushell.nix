{ lib, config, ... }:

let
  cfg = config.presets.lsp.servers.nushell;
in

{
  options.presets.lsp.servers.nushell = {
    enable = lib.mkEnableOption "Use nushell language server";
  };

  config.lsp.servers.nushell = lib.mkIf cfg.enable {
    server = "nu";
    args = [ "--lsp" ];
    filetypes = [ "nu" ];
    root.patterns = [ ".git/" ];
  };
}
