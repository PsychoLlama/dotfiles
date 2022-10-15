{ config, nixpkgs-unstable, lib, ... }:

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

  config = with lib;
    mkIf cfg.enable {
      environment.systemPackages = with nixpkgs-unstable; [
        nodejs-16_x
        pastel
        yarn
      ];
    };
}
