{ units, module, ... }:

let
  inherit (units.platform) nixos;
in

{
  unit = module.component "unit" {
    config = [
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
