{
  # Nushell ships its own language server, so the binary is whichever `nu` is
  # on PATH rather than a package this preset installs.
  platforms.editor.lsp.servers.nushell = {
    cmd = [
      "nu"
      "--lsp"
    ];
    filetypes = [ "nu" ];
    root_markers = [ ".git/" ];
  };
}
