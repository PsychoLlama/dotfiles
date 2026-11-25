{ rhizome, units, ... }:

let
  inherit (units.platform) nixos;
in

rhizome.component "wireshark" [
  (nixos.module (
    { pkgs, ... }:

    {
      programs.wireshark = {
        enable = true;
        package = pkgs.unstable.wireshark;
      };
    }
  ))
]
