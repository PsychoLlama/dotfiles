{ units, module, ... }:

let
  inherit (units.programs) claude-code;
in

{
  unit = module.component "unit" {
    config = [
      (claude-code.plugin (
        {
          lib,
          config,
          pkgs,
          ...
        }:

        {

          name = "typescript-lsp";
          description = "TypeScript Language Server for Claude Code.";

          lsp.typescript = {
            command = lib.getExe' pkgs.unstable.typescript-language-server "typescript-language-server";
            args = [ "--stdio" ];
            extensionToLanguage = {
              ".ts" = "typescript";
              ".tsx" = "typescriptreact";
            };
          };
        }
      ))
    ];
  };
}
