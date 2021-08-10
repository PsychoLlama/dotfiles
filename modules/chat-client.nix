{ config, unstable, lib, ... }:

let
  df = config.dotfiles;
  cfg = df.chat-client;

in {
  options = with lib; {
    dotfiles.chat-client = {
      enable = mkOption {
        type = types.bool;
        description = "Enable the weechat client";
        default = df.kitchen-sink.enable;
      };

      matrix.enable = mkOption {
        type = types.bool;
        description = "Enable the matrix-weechat integration";
        default = df.kitchen-sink.enable;
      };

      slack.enable = mkOption {
        type = types.bool;
        description = "Enable the slack-weechat integration";
        default = df.kitchen-sink.enable;
      };
    };
  };

  config = with lib; {
    users.users.${df.user.account}.packages = mkIf cfg.enable [
      (unstable.weechat.override {
        configure = { ... }: {
          scripts = with unstable.weechatScripts;
            (if cfg.matrix.enable then [ weechat-matrix ] else [ ])
            ++ (if cfg.slack.enable then [ wee-slack ] else [ ]);
        };
      })
    ];
  };
}
