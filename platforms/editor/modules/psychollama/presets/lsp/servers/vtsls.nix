{
  lib,
  config,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.lsp.servers.vtsls;
in

{
  options.psychollama.presets.lsp.servers.vtsls = {
    enable = lib.mkEnableOption "Use vtsls (VS Code TypeScript language server)";
    package = lib.mkPackageOption pkgs.unstable "vtsls" { };
  };

  config = lib.mkIf cfg.enable {
    lsp.servers.vtsls = {
      server = "${cfg.package}/bin/vtsls";
      args = [ "--stdio" ];
      filetypes = [
        "typescript"
        "typescriptreact"
        "javascript"
        "javascriptreact"
      ];

      root.patterns = [
        "tsconfig.json"
        "package.json"
        ".git/"
      ];
    };

    extraPackages = [
      pkgs.unstable.unzip
    ];
  };
}
