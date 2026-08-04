{
  lib,
  config,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.lsp.servers.clangd;
in

{
  options.psychollama.presets.lsp.servers.clangd = {
    enable = lib.mkEnableOption "Use clangd language server";
    package = lib.mkPackageOption pkgs.unstable "clang-tools" { };
  };

  config.lsp.servers.clangd = lib.mkIf cfg.enable {
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
