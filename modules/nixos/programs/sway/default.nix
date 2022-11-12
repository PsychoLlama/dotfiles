{ config, lib, ... }:

with lib;

let cfg = config.programs.sway;

in {
  imports = [ ./input.nix ];

  options.programs.sway = {
    extraConfig = mkOption {
      type = types.lines;
      description = "Lines to append to the Sway config file";
      default = "";
    };
  };

  config.environment.etc."sway/config" = {
    enable = stringLength cfg.extraConfig > 0;
    text = cfg.extraConfig;
  };
}
