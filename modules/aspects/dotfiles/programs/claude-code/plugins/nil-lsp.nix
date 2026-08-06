{ dotfiles, ... }:

{
  dotfiles.programs.claude-code.plugins.nil-lsp.homeManager =
    { lib, pkgs, ... }:

    {
      programs.claude-code.plugins.nil-lsp = {
        description = "Nil (nix) Language Server for Claude Code.";

        lsp.servers.nil = {
          command = lib.getExe pkgs.unstable.nil;
          extensionToLanguage.".nix" = "nix";
        };
      };
    };
}
