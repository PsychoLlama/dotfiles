{ rhizome, units, ... }:

let
  inherit (units.platform) nixos user;
in

rhizome.component "network-manager" {
  uses = {
    identity = user.identity;
  };

  config = { identity }: [
    (nixos.module (
      { lib, ... }:

      let
        inherit (identity) username;
      in

      {
        networking.networkmanager.enable = lib.mkDefault true;

        # Lets the owner switch networks without sudo.
        users.users.${username}.extraGroups = [ "networkmanager" ];
      }
    ))
  ];
}
