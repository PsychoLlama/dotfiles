{ dotfiles, ... }:

{
  dotfiles.programs.claude-code.plugins.nushell-lsp.homeManager = {
    programs.claude-code.plugins.nushell-lsp = {
      description = "Nushell Language Server for Claude Code.";

      lsp.servers.nushell = {
        command = "nu";
        args = [ "--lsp" ];
        extensionToLanguage.".nu" = "nu";
      };
    };
  };
}
