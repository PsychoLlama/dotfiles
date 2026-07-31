{
  config.programs.claude-code.marketplace.plugins.rust-lsp = {
    description = "Rust Language Server for Claude Code.";

    lsp.servers.rust = {
      command = "rust-analyzer";
      extensionToLanguage.".rs" = "rust";
    };
  };
}
