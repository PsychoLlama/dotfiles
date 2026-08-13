{
  exports.editor.lsp.servers.rust-analyzer = {
    cmd = [ "rust-analyzer" ];
    filetypes = [ "rust" ];
    root_markers = [
      "Cargo.lock"
      "Cargo.toml"
    ];
  };
}
