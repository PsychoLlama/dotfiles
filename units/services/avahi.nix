{ rhizome, units, ... }:

let
  inherit (units.platform) nixos;
in

rhizome.unit "avahi" {
  unit = rhizome.component [
    (nixos.module (
      { config, lib, ... }:

      {
        services.avahi = {
          enable = lib.mkDefault true;
          nssmdns4 = lib.mkDefault true;
        };
      }
    ))
  ];
}
