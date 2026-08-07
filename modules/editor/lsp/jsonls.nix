{
  flake.modules.editor.default =
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
        package = lib.mkPackageOption pkgs.unstable "vscode-langservers-extracted" { };
      };

      config.lsp.servers.jsonls = {
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
    };
}
