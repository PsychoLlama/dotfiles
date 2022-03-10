{ config, unstable, lib, ... }:

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
    with unstable;
    mkMerge [
      (mkIf cfg.enable {
        environment.systemPackages = [ bottom man-pages ncspot rage ];
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
