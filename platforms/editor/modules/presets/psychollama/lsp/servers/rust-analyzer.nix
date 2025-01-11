{
  lib,
  config,
  pkgs,
  ...
}:

let
  cfg = config.presets.lsp.servers.rust-analyzer;
in

{
  options.presets.lsp.servers.rust-analyzer = {
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
