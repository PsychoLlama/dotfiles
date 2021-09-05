{ config, unstable, lib, ... }:

let
  df = config.dotfiles;
  cfg = df.toolkit.js-development;

in {
  options.dotfiles.toolkit.js-development = with lib; {
    enable = mkOption {
      type = types.bool;
      description = "Enable toolkit for JavaScript development";
      default = df.kitchen-sink.enable;
    };
  };

  config = with lib; {
    environment.systemPackages = with unstable;
      mkIf cfg.enable [ nodejs-16_x pastel yarn ];
  };
}
