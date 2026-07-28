{
  cfg,
  lib,
  pkgs,
  ...
}:

let
  viu = (cfg.package.override { withSixel = cfg.sixel.enable; }).overrideAttrs (prev: {
    # viu's `sixel-sys` crate builds a vendored libsixel from source, which
    # needs the autotools toolchain on PATH. Upstream omits it.
    nativeBuildInputs =
      (prev.nativeBuildInputs or [ ])
      ++ lib.optionals cfg.sixel.enable [
        pkgs.autoconf
        pkgs.automake
        pkgs.libtool
        pkgs.pkg-config
      ];
  });
in

{
  options = {
    package = lib.mkPackageOption pkgs.unstable "viu" { };

    sixel.enable = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to build viu with sixel support.";
    };
  };

  modules.home-manager.home.packages = [ viu ];
}
