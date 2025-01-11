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

    # Compiling stuff with XCode is a pain. Just use the host toolkit.
    system = lib.mkOption {
      type = lib.types.bool;
      default = pkgs.stdenv.isDarwin;
      description = "Pull `clangd` from the host system";
    };
  };

  config.lsp.servers.clangd = lib.mkIf cfg.enable {
    server = if cfg.system then "clangd" else "${cfg.package}/bin/clangd";
    root.patterns = [ "compile_commands.json" ];
    filetypes = [
      "c"
      "cpp"
      "objc"
      "objcpp"
    ];
  };
}
