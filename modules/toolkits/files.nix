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
        environment.variables.FZF_DEFAULT_COMMAND = "fd";

        environment.systemPackages = with nixpkgs-unstable; [
          binutils
          du-dust
          glow
          hexyl
          ipfs
          jq
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
