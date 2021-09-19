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

      brave = {
        enable = mkOption {
          type = types.bool;
          description = "Enable the Brave browser";
          default = df.kitchen-sink.enable;
        };

        package = mkOption {
          type = types.package;
          description = "Which Brave package to use";
          default = unstable.brave;
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

  config = with lib; {
    environment.systemPackages =
      (if browsers.firefox.enable then [ browsers.firefox.package ] else [ ])
      ++ (if browsers.tor.enable then [ browsers.tor.package ] else [ ])
      ++ (if browsers.brave.enable then [ browsers.brave.package ] else [ ])
      ++ (if cfg.zathura.enable then [ cfg.zathura.package ] else [ ]);
  };
}
