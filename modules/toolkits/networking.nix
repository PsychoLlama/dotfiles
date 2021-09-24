{ config, unstable, lib, ... }:

let
  df = config.dotfiles;
  cfg = df.toolkit.networking;

in {
  options.dotfiles.toolkit.networking = with lib; {
    enable = mkOption {
      type = types.bool;
      description = "Enable networking tool bundle";
      default = df.kitchen-sink.enable;
    };

    w3m.keymap = mkOption {
      type = types.path;
      description = "Set the w3m keymap file";
      default = ../../config/w3m.keymap;
    };
  };

  config = with lib;
    mkIf cfg.enable {
      programs.wireshark.enable = true;
      environment.systemPackages = with unstable; [
        (unstable.callPackage ../../pkgs/w3m.nix { keymap = cfg.w3m.keymap; })
        dogdns
        nmap
        termshark
        whois
        xh
      ];
    };
}
