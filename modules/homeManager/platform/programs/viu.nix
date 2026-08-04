{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.viu;
  pkg = (cfg.package.override { withSixel = cfg.withSixelSupport; }).overrideAttrs (prev: {
    # viu's `sixel-sys` crate builds a vendored libsixel from source, which
    # needs the autotools toolchain on PATH. Upstream omits it.
    nativeBuildInputs =
      (prev.nativeBuildInputs or [ ])
      ++ lib.optionals cfg.withSixelSupport [
        pkgs.autoconf
        pkgs.automake
        pkgs.libtool
        pkgs.pkg-config
      ];
  });
in

{
  options.programs.viu = {
    enable = lib.mkEnableOption "Whether to install viu";
    package = lib.mkPackageOption pkgs "viu" { };
    withSixelSupport = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = "Whether to enable sixel support in viu";
    };
  };

  config.home.packages = lib.mkIf cfg.enable [ pkg ];
}
