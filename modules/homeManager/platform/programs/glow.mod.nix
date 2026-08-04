{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.programs.glow;
  yaml = pkgs.formats.yaml { };
in

{
  options.programs.glow = {
    enable = lib.mkEnableOption "Enable the Glow markdown viewer";
    package = lib.mkPackageOption pkgs "glow" { };

    settings = lib.mkOption {
      type = yaml.type;
      description = "Configuration to include in the glow config file";
      default = { };
    };
  };

  config = lib.mkIf cfg.enable {
    home.packages = [ cfg.package ];
    xdg.configFile."glow/glow.yml".source = yaml.generate "glow-config" cfg.settings;
  };
}
