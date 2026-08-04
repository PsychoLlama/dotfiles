{ lib, config, ... }:

let
  cfg = config.psychollama.presets.lsp.servers.rust-analyzer;
in

{
  options.psychollama.presets.lsp.servers.rust-analyzer = {
    enable = lib.mkEnableOption "Use the Rust language server";
  };

  config.lsp.servers.rust-analyzer = lib.mkIf cfg.enable {
    cmd = [ "rust-analyzer" ];
    filetypes = [ "rust" ];
    root_markers = [
      "Cargo.lock"
      "Cargo.toml"
    ];
  };
}
