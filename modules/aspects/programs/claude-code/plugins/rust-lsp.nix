{
  exports.homeManager.programs.claude-code.plugins.rust-lsp = {
    description = "Rust Language Server for Claude Code.";

    lsp.servers.rust = {
      command = "rust-analyzer";
      extensionToLanguage.".rs" = "rust";
    };
  };
}
