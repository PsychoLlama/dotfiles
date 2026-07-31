{
  config.programs.claude-code.marketplace.plugins.nushell-lsp = {
    description = "Nushell Language Server for Claude Code.";

    lsp.servers.nushell = {
      command = "nu";
      args = [ "--lsp" ];
      extensionToLanguage.".nu" = "nu";
    };
  };
}
