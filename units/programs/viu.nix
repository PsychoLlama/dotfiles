{ units, define, ... }:

{
  unit = define.unit {
    provides = [
      (units.platform.home-manager.module (
        { pkgs, ... }:

        {
          home.packages = [
            ((pkgs.viu.override { withSixel = true; }).overrideAttrs (prev: {
              # viu's `sixel-sys` crate builds a vendored libsixel from source, which
              # needs the autotools toolchain on PATH. Upstream omits it.
              nativeBuildInputs = (prev.nativeBuildInputs or [ ]) ++ [
                pkgs.autoconf
                pkgs.automake
                pkgs.libtool
                pkgs.pkg-config
              ];
            }))
          ];
        }
      ))
    ];
  };
}
