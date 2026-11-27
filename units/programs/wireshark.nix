{ units, define, ... }:

let
  inherit (units.platform) nixos;
in

{
  unit = define.unit {
    provides = [
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
