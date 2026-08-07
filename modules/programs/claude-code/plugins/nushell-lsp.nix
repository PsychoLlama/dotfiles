{
  flake.modules.homeManager.default.programs.claude-code.plugins.nushell-lsp = {
    description = "Nushell Language Server for Claude Code.";

    lsp.servers.nushell = {
      command = "nu";
      args = [ "--lsp" ];
      extensionToLanguage.".nu" = "nu";
    };
  };
}
