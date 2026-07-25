{
  platforms.home-manager.programs.claude-code.localPlugins.rust-lsp = {
    description = "Rust Language Server for Claude Code.";

    lsp.servers.rust = {
      command = "rust-analyzer";
      extensionToLanguage.".rs" = "rust";
    };
  };
}
