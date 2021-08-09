{ config, unstable, lib, ... }:

let cfg = config.dotfiles.toolkit.networking;

in {
  options.dotfiles.toolkit.networking = with lib; {
    enable = mkOption {
      type = types.bool;
      description = "Enable networking tool bundle";
      default = true;
    };

    w3m.keymap = mkOption {
      type = types.path;
      description = "Set the w3m keymap file";
      default = ../config/w3m.keymap;
    };
  };

  config = with lib; {
    programs.wireshark.enable = mkIf cfg.enable true;
    environment.systemPackages = with unstable;
      mkIf cfg.enable [
        (unstable.callPackage ../pkgs/w3m.nix { keymap = cfg.w3m.keymap; })
        dogdns
        nmap
        termshark
        whois
        xh
      ];
  };
}
