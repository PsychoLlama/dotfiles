{ config, nixpkgs-unstable, lib, ... }:

let
  df = config.dotfiles;
  cfg = df.apps;
  browsers = cfg.browsers;

in {
  options.dotfiles.apps = with lib; {
    zathura = {
      enable = mkOption {
        type = types.bool;
        description = "Enable the Zathura PDF viewer";
        default = df.kitchen-sink.enable;
      };

      package = mkOption {
        type = types.package;
        description = "Which zathura package to use";
        default = nixpkgs-unstable.zathura;
      };
    };
  };

  config = with lib;
    mkMerge [
      (mkIf cfg.zathura.enable {
        environment.systemPackages = [ cfg.zathura.package ];
      })
    ];
}
