{
  lib,
  config,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.lsp.servers.jsonls;
in

{
  options.psychollama.presets.lsp.servers.jsonls = {
    enable = lib.mkEnableOption "Use JSON language server";
    package = lib.mkPackageOption pkgs.unstable "vscode-langservers-extracted" { };
  };

  config.lsp.servers.jsonls = lib.mkIf cfg.enable {
    server = "${cfg.package}/bin/vscode-json-language-server";
    root.patterns = [ ".git/" ];
    args = [ "--stdio" ];
    filetypes = [
      "json"
      "jsonc"
      "json5"
    ];
  };
}
