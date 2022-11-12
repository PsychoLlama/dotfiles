{ config, lib, pkgs, ... }:

let
  df = config.dotfiles;
  cfg = df.toolkits;

in with lib; {
  options.dotfiles.toolkits = {
    files.enable = mkEnableOption "Enable file navigation/inspection toolkit";

    system = {
      enable = mkOption {
        type = types.bool;
        description = "Whether to enable the system toolkit";
        default = df.kitchen-sink.enable;
      };
    };
  };

  config = with lib;
    mkMerge [
      (mkIf cfg.files.enable {
        environment.systemPackages = with pkgs.unstable; [
          binutils
          du-dust
          glow
          hexyl
          ipfs
          litecli
          lnav
          pv
          ripgrep
          tokei
          viu
        ];
      })

      (mkIf cfg.system.enable {
        environment.systemPackages = with pkgs.unstable; [ man-pages rage duf ];
      })
    ];
}
