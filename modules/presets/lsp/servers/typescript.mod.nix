{
  cfg,
  lib,
  pkgs,
  ...
}:

{
  options.package = lib.mkPackageOption pkgs.unstable "typescript-language-server" { };

  platforms.editor = {
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
}
