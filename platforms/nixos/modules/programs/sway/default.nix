{ config, lib, ... }:

let
  cfg = config.programs.sway;
in

{
  options.programs.sway = {
    extraConfig = lib.mkOption {
      type = lib.types.lines;
      description = "Lines to append to the Sway config file";
      default = "";
    };
  };

  config.environment.etc."sway/config" = {
    enable = lib.stringLength cfg.extraConfig > 0;
    text = cfg.extraConfig;
  };
}
