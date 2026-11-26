{ rhizome, units, ... }:

let
  inherit (units.platform) nixos;
in

rhizome.unit "wireshark" {
  unit = rhizome.component [
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
}
