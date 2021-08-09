{ config, unstable, lib, ... }:

let cfg = config.dotfiles.toolkit.networking;

in {
  options.dotfiles.toolkit.networking = with lib; {
    enable = mkOption {
      type = types.bool;
      description = "Enable networking tool bundle";
      default = true;
    };
  };

  config = with lib; {
    programs.wireshark.enable = mkIf cfg.enable true;
    environment.systemPackages = with unstable;
      mkIf cfg.enable [ dogdns nmap termshark whois xh ];
  };
}
