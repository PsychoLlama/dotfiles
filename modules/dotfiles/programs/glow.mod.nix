{
  cfg,
  lib,
  pkgs,
  ...
}:

let
  yaml = pkgs.formats.yaml { };
in

{
  options = {
    package = lib.mkPackageOption pkgs.unstable "glow" { };

    settings = lib.mkOption {
      type = yaml.type;
      description = "Configuration to write to glow's config file.";
      default = {
        local = true;
        pager = false;
      };
    };
  };

  modules.home-manager = {
    home.packages = [ cfg.package ];
    xdg.configFile."glow/glow.yml".source = yaml.generate "glow-config" cfg.settings;
  };
}
