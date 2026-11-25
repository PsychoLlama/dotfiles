{ rhizome, units, ... }:

let
  inherit (units.platform) nixos;
in

rhizome.component "avahi" [
  (nixos.module (
    { config, lib, ... }:

    {
      services.avahi = {
        enable = lib.mkDefault true;
        nssmdns4 = lib.mkDefault true;
      };
    }
  ))
]
