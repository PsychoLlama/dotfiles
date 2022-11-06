{ config, nixpkgs-unstable, lib, ... }:

let
  df = config.dotfiles;
  cfg = df.toolkit.files;

in {
  options.dotfiles.toolkit.files = with lib; {
    enable = mkOption {
      type = types.bool;
      description = "Enable file navigation/inspection toolkit";
      default = df.kitchen-sink.enable;
    };
  };

  config = with lib;
    mkMerge [
      (mkIf cfg.enable {
        environment.systemPackages = with nixpkgs-unstable; [
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
    ];
}
