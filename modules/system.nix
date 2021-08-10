{ config, unstable, lib, ... }:

let cfg = config.dotfiles.toolkit.system;

in {
  options.dotfiles.toolkit.system = with lib; {
    enable = mkOption {
      type = types.bool;
      description = "Whether to enable the system toolkit";
      default = true;
    };
  };

  config = with lib; {
    environment.systemPackages = with unstable;
      mkIf cfg.enable [ acpi bottom ncspot playerctl rage scrot xclip ];
  };
}
