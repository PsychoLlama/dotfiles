{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.somo;
in

{
  options.programs.somo = {
    enable = lib.mkEnableOption "Whether to install somo";
    package = lib.mkPackageOption pkgs "somo" { };

    config = lib.mkOption {
      type = lib.types.lines;
      default = "";
      description = ''
        Contents of somo's config file. Each line is either a CLI flag
        (e.g. `--compact`) or a `#`-prefixed comment. When non-empty, the
        contents are written to `$XDG_CONFIG_HOME/somo/config`, the only
        location somo reads.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];

    xdg.configFile."somo/config" = lib.mkIf (cfg.config != "") {
      text = cfg.config;
    };
  };
}
