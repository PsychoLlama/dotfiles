{ config, nixpkgs-unstable, lib, ... }:

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
  };

  config = with lib;
    mkIf cfg.enable {
      programs.wireshark.enable = true;
      environment.systemPackages = with nixpkgs-unstable; [
        dogdns
        nmap
        termshark
        whois
        xh
      ];
    };
}
