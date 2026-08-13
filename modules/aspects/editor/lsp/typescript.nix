{
  flake.modules.editor.default =
    {
      lib,
      config,
      pkgs,
      ...
    }:

    let
      cfg = config.psychollama.presets.lsp.servers.typescript;
    in

    {
      options.psychollama.presets.lsp.servers.typescript = {
        package = lib.mkPackageOption pkgs.unstable "typescript-language-server" { };
      };

      config = {
        lsp.servers.typescript = {
          cmd = [
            "${cfg.package}/bin/typescript-language-server"
            "--stdio"
          ];
          filetypes = [
            "typescript"
            "typescriptreact"
            "javascript"
            "javascriptreact"
          ];

          root_markers = [
            "tsconfig.json"
            ".git/"
          ];
        };

        extraPackages = [
          pkgs.unstable.unzip
          pkgs.unstable.yarn
        ];
      };
    };
}
