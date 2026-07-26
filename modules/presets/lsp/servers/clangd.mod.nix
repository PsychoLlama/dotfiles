{
  cfg,
  lib,
  pkgs,
  ...
}:

{
  options.package = lib.mkPackageOption pkgs.unstable "clang-tools" { };

  platforms.editor.lsp.servers.clangd = {
    cmd = [ "${cfg.package}/bin/clangd" ];
    root_markers = [ "compile_commands.json" ];
    filetypes = [
      "c"
      "cpp"
      "objc"
      "objcpp"
    ];
  };
}
