{ config, pkgs, lib, ... }:

let
  df = config.dotfiles;
  cfg = config.dotfiles.desktop;

in with lib; {
  options.dotfiles.desktop = {
    enable = mkOption {
      type = types.bool;
      description = "Enable a desktop environment";
      default = df.kitchen-sink.enable;
    };

    sway.config = mkOption {
      type = types.path;
      description = "Set the Sway config file";
      default = ../config/sway.conf;
    };
  };

  config = mkIf cfg.enable {
    # Enable a minimal desktop environment with Sway/Wayland.
    programs.sway.enable = true;
    dotfiles.user.packages = [ pkgs.unstable.wlsunset ];
    environment.etc."sway/config".source = cfg.sway.config;
  };
}
