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

    sway.config = mkOption {
      type = types.path;
      description = "Set the XMonad config file";
      default = ../config/sway.conf;
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

      environment.systemPackages = [
        (unstable.callPackage ../pkgs/rofi.nix { configDir = cfg.rofi.config; })
      ];

      environment.etc."sway/config".source = cfg.sway.config;

      # Automatically start Sway session on login.
      environment.loginShellInit = ''
        test "$(tty)" = /dev/tty1 && exec sway
      '';

      # If they're enabling a desktop, these seem like reasonable defaults.
      services.printing.enable = mkDefault true;
      sound.enable = mkDefault true;
      hardware.pulseaudio.enable = mkDefault true;
    };
}
