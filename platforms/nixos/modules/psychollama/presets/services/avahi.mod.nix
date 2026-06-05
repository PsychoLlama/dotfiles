{ config, lib, ... }:

let
  cfg = config.psychollama.presets.services.avahi;
in
{
  options.psychollama.presets.services.avahi = {
    enable = lib.mkEnableOption "Enable mDNS service discovery";
  };

  config = lib.mkIf cfg.enable {
    services.avahi = {
      enable = lib.mkDefault true;
      nssmdns4 = lib.mkDefault true;
    };
  };
}
