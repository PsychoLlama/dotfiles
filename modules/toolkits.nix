{ config, lib, nixpkgs-unstable, ... }:

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

      linux.enable = mkOption {
        type = types.bool;
        description = "Add tools for managing Linux";
        default = df.kitchen-sink.enable;
      };
    };
  };

  config = with lib;
    with nixpkgs-unstable;
    mkMerge [
      (mkIf cfg.files.enable {
        environment.systemPackages = [
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
        environment.systemPackages = [ man-pages rage duf ];
      })

      (mkIf cfg.system.linux.enable {
        environment.systemPackages = [
          acpi
          brightnessctl
          grim
          parted
          playerctl
          slurp
          wf-recorder
          wl-clipboard
        ];
      })
    ];
}
