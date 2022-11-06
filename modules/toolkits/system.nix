{ config, nixpkgs-unstable, lib, ... }:

let
  df = config.dotfiles;
  cfg = df.toolkit.system;

in {
  options.dotfiles.toolkit.system = with lib; {
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

  config = with lib;
    with nixpkgs-unstable;
    mkMerge [
      (mkIf cfg.enable {
        environment.systemPackages = [ man-pages ncspot rage duf ];
      })

      (mkIf cfg.linux.enable {
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
