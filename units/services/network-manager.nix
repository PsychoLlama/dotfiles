{ rhizome, units, ... }:

let
  inherit (units.platform) nixos;
in

rhizome.component "network-manager" [
  (nixos.module (
    { host, lib, ... }:

    let
      inherit (host.identity) username;
    in

    {
      networking.networkmanager.enable = lib.mkDefault true;

      # Lets the owner switch networks without sudo.
      users.users.${username}.extraGroups = [ "networkmanager" ];
    }
  ))
]
