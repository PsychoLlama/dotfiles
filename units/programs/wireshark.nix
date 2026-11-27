{ units, module, ... }:

let
  inherit (units.platform) nixos;
in

{
  unit = module.component "unit" {
    config = [
      (nixos.module (
        { pkgs, ... }:

        {
          programs.wireshark = {
            enable = true;
            package = pkgs.unstable.wireshark;
          };
        }
      ))
    ];
  };
}
