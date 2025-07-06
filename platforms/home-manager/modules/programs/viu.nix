{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.viu;
  pkg = cfg.package.override {
    withSixel = cfg.withSixelSupport;
  };
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
