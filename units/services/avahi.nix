{ units, define, ... }:

let
  inherit (units.platform) nixos;
in

{
  unit = define.unit {
    provides = [
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
  };
}
