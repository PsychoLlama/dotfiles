{
  lib,
  config,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.presets.lsp.servers.rust-analyzer;
in

{
  options.psychollama.presets.lsp.servers.rust-analyzer = {
    enable = lib.mkEnableOption "Use the Rust language server";
    package = lib.mkPackageOption pkgs.unstable "rust-analyzer" { };
  };

  config = lib.mkIf cfg.enable {
    lsp.servers.rust-analyzer = {
      server = "${cfg.package}/bin/rust-analyzer";
      filetypes = [ "rust" ];
      root.patterns = [ "Cargo.toml" ];
    };

    extraPackages = [
      pkgs.unstable.rustup
    ];
  };
}
