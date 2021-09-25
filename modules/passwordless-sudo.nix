{ config, lib, ... }:

let
  df = config.dotfiles;
  cfg = df.passwordless-sudo;

in {
  options.dotfiles.passwordless-sudo = with lib; {
    enable = mkOption {
      type = types.bool;
      description = "Enable passwordless sudo";
      default = df.kitchen-sink.enable;
    };
  };

  config = with lib;
    mkIf cfg.enable { security.sudo.wheelNeedsPassword = false; };
}
