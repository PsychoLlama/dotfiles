{ config, unstable, lib, pkgs, ... }:

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

  config = with lib;
    mkIf cfg.enable {
      environment.systemPackages = with unstable; [
        cargo-edit
        gcc
        openssl.dev
        pkg-config
        rustup
      ];

      # Several popular Rust libraries depend on OpenSSL bindings.
      environment.variables = {
        PKG_CONFIG_PATH = "${unstable.openssl.dev}/lib/pkgconfig";
      };
    };
}
