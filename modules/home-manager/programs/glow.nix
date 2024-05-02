{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  cfg = config.programs.glow;
  yaml = pkgs.formats.yaml { };
in
{
  options.programs.glow = {
    enable = mkEnableOption "Enable the Glow markdown viewer";
    package = mkPackageOption pkgs "glow" { };

    settings = mkOption {
      type = yaml.type;
      description = "Configuration to include in the glow config file";
      default = { };
    };
  };

  config = mkIf cfg.enable {
    home.packages = [ cfg.package ];
    xdg.configFile."glow/glow.yml".source = yaml.generate "glow-config" cfg.settings;
  };
}
