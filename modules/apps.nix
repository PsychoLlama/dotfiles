{ config, unstable, lib, ... }:

let
  df = config.dotfiles;
  cfg = df.apps;
  browsers = cfg.browsers;

in {
  options.dotfiles.apps = with lib; {
    browsers = {
      firefox = {
        enable = mkOption {
          type = types.bool;
          description = "Enable Firefox";
          default = df.kitchen-sink.enable;
        };

        package = mkOption {
          type = types.package;
          description = "Which firefox package to use";
          default = unstable.firefox;
        };
      };

      tor = {
        enable = mkOption {
          type = types.bool;
          description = "Enable the Tor browser";
          default = df.kitchen-sink.enable;
        };

        package = mkOption {
          type = types.package;
          description = "Which Tor browser package to use";
          default = unstable.torbrowser;
        };
      };
    };

    zathura = {
      enable = mkOption {
        type = types.bool;
        description = "Enable the Zathura PDF viewer";
        default = df.kitchen-sink.enable;
      };

      package = mkOption {
        type = types.package;
        description = "Which zathura package to use";
        default = unstable.zathura;
      };
    };
  };

  config = with lib;
    mkMerge [
      (mkIf browsers.firefox.enable {
        environment.systemPackages = [ browsers.firefox.package ];
      })
      (mkIf browsers.tor.enable {
        environment.systemPackages = [ browsers.tor.package ];
      })
      (mkIf cfg.zathura.enable {
        environment.systemPackages = [ cfg.zathura.package ];
      })
    ];
}
