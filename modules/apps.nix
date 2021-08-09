{ config, unstable, lib, ... }:

let
  df = config.dotfiles;
  cfg = df.apps;

in {
  options.dotfiles.apps = with lib; {
    firefox = {
      enable = mkOption {
        type = types.bool;
        description = "Enable Firefox";
        default = true;
      };

      package = mkOption {
        type = types.package;
        description = "Which firefox package to use";
        default = unstable.firefox;
      };
    };

    torbrowser = {
      enable = mkOption {
        type = types.bool;
        description = "Enable the Tor browser";
        default = true;
      };

      package = mkOption {
        type = types.package;
        description = "Which Tor browser package to use";
        default = unstable.torbrowser;
      };
    };

    zathura = {
      enable = mkOption {
        type = types.bool;
        description = "Enable the Zathura PDF viewer";
        default = true;
      };

      package = mkOption {
        type = types.package;
        description = "Which zathura package to use";
        default = unstable.zathura;
      };
    };
  };

  config = with lib; {
    users.users.${df.user.account}.packages =
      (if cfg.firefox.enable then [ cfg.firefox.package ] else [ ])
      ++ (if cfg.torbrowser.enable then [ cfg.torbrowser.package ] else [ ])
      ++ (if cfg.zathura.enable then [ cfg.zathura.package ] else [ ]);
  };
}
