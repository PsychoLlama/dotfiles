{
  cfg,
  lib,
  pkgs,
  ...
}:

{
  options.package = lib.mkPackageOption pkgs.unstable "vscode-langservers-extracted" { };

  platforms.editor.lsp.servers.jsonls = {
    cmd = [
      "${cfg.package}/bin/vscode-json-language-server"
      "--stdio"
    ];
    root_markers = [ ".git/" ];
    filetypes = [
      "json"
      "jsonc"
      "json5"
    ];
  };
}
