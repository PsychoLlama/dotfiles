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
    enable = lib.mkEnableOption "Use TypeScript language server";
    package = lib.mkPackageOption pkgs.unstable.nodePackages "typescript-language-server" { };
  };

  config = lib.mkIf cfg.enable {
    lsp.servers.typescript = {
      server = "${cfg.package}/bin/typescript-language-server";
      args = [ "--stdio" ];
      filetypes = [
        "typescript"
        "typescriptreact"
        "javascript"
        "javascriptreact"
      ];

      root.patterns = [
        "tsconfig.json"
        ".git/"
      ];
    };

    extraPackages = [
      pkgs.unstable.unzip # For source-diving Plug'n'Play dependencies.
      pkgs.unstable.yarn
    ];
  };
}
