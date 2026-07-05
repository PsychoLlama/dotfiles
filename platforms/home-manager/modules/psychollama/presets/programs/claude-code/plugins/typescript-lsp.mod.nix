{
  lib,
  config,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.programs.claude-code.plugins.typescript-lsp;
  tsLanguageServer = lib.getExe' cfg.package "typescript-language-server";
in

{
  options.psychollama.presets.programs.claude-code.plugins.typescript-lsp = {
    package = lib.mkPackageOption pkgs.unstable "typescript-language-server" { };
    enable = lib.mkEnableOption "TypeScript LSP for Claude Code" // {
      default = true;
    };
  };

  config = lib.mkIf cfg.enable {
    programs.claude-code.localPlugins.typescript-lsp = {
      description = "TypeScript Language Server for Claude Code.";

      lsp.servers.typescript = {
        command = tsLanguageServer;
        args = [ "--stdio" ];
        extensionToLanguage = {
          ".ts" = "typescript";
          ".tsx" = "typescriptreact";
        };
      };
    };
  };
}
