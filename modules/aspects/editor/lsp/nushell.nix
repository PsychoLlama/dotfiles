{
  exports.editor.lsp.servers.nushell = {
    cmd = [
      "nu"
      "--lsp"
    ];
    filetypes = [ "nu" ];
    root_markers = [ ".git/" ];
  };
}
