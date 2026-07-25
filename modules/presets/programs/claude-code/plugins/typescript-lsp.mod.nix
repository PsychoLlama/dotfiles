{
  cfg,
  lib,
  pkgs,
  ...
}:

let
  tsLanguageServer = lib.getExe' cfg.package "typescript-language-server";
in

{
  options = {
    package = lib.mkPackageOption pkgs.unstable "typescript-language-server" { };
  };

  platforms.home-manager.programs.claude-code.localPlugins.typescript-lsp = {
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
}
