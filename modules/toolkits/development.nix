{ config, nixpkgs-unstable, lib, pkgs, ... }:

let
  df = config.dotfiles;
  cfg = df.toolkit.development;
  ini = pkgs.formats.ini { };

in {
  options.dotfiles.toolkit.development = with lib; {
    enable = mkOption {
      type = types.bool;
      description = "Enable the development toolkit";
      default = df.kitchen-sink.enable;
    };
  };

  config = with lib;
    mkMerge [
      (mkIf cfg.enable {
        environment.systemPackages = with nixpkgs-unstable; [ sshfs ];
      })
    ];
}
