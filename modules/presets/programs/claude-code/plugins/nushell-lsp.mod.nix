{
  platforms.home-manager.programs.claude-code.localPlugins.nushell-lsp = {
    description = "Nushell Language Server for Claude Code.";

    lsp.servers.nushell = {
      command = "nu";
      args = [ "--lsp" ];
      extensionToLanguage.".nu" = "nu";
    };
  };
}
