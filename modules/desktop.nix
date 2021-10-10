{ config, unstable, lib, ... }:

let
  df = config.dotfiles;
  cfg = config.dotfiles.desktop;

in {
  options.dotfiles.desktop = with lib; {
    enable = mkOption {
      type = types.bool;
      description = "Enable a desktop environment";
      default = df.kitchen-sink.enable;
    };

    xmonad.config = mkOption {
      type = types.path;
      description = "Set the XMonad config file";
      default = ../config/xmonad.hs;
    };

    rofi.config = mkOption {
      type = types.path;
      description = "Set the Rofi configuration directory";
      default = ../config/rofi;
    };
  };

  config = with lib;
    mkIf cfg.enable {
      # Enable a minimal desktop environment with Sway/Wayland.
      programs.sway = {
        enable = true;
        extraSessionCommands = ''
          # Remap caps lock to escape.
          export XKB_DEFAULT_OPTIONS=caps:escape
        '';
      };
    };
}
