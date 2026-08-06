{ dotfiles, ... }:

{
  dotfiles.programs.claude-code.plugins.typescript-lsp.homeManager =
    { lib, pkgs, ... }:

    {
      programs.claude-code.plugins.typescript-lsp = {
        description = "TypeScript Language Server for Claude Code.";

        lsp.servers.typescript = {
          command = lib.getExe' pkgs.unstable.typescript-language-server "typescript-language-server";
          args = [ "--stdio" ];
          extensionToLanguage = {
            ".ts" = "typescript";
            ".tsx" = "typescriptreact";
          };
        };
      };
    };
}
