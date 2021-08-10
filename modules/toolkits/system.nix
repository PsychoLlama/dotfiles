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
  };

  config = with lib; {
    environment.systemPackages = with unstable;
      mkIf cfg.enable [ acpi bottom ncspot playerctl rage scrot xclip ];
  };
}
