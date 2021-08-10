{ config, unstable, lib, ... }:

let
  df = config.dotfiles;
  cfg = df.toolkit.rust-development;

in {
  options.dotfiles.toolkit.rust-development = with lib; {
    enable = mkOption {
      type = types.bool;
      description = "Enable the Rust development toolkit";
      default = df.kitchen-sink.enable;
    };
  };

  config = with lib; {
    environment.systemPackages = with unstable;
      mkIf cfg.enable [
        cargo
        cargo-edit
        clippy
        gcc
        openssl.dev
        pkg-config
        rls
        rustc
        rustup
      ];

    # Several popular Rust libraries depend on OpenSSL bindings.
    environment.variables = mkIf cfg.enable {
      PKG_CONFIG_PATH = "${unstable.openssl.dev}/lib/pkgconfig";
    };
  };
}
