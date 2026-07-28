{
  # `rust-analyzer` is a dynamic dependency: it comes from the project's dev
  # shell so it matches the toolchain the project builds with.
  modules.editor.lsp.servers.rust-analyzer = {
    cmd = [ "rust-analyzer" ];
    filetypes = [ "rust" ];
    root_markers = [
      "Cargo.lock"
      "Cargo.toml"
    ];
  };
}
