{ units, define, ... }:

let
  inherit (units.platform) nixos user;
in

{
  unit = define.unit {
    uses = {
      inherit (user) identity;
    };

    provides = { identity }: [
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
  };
}
