{ units, module, ... }:

let
  inherit (units.platform) nixos user;
in

{
  unit = module.component {
    uses = {
      inherit (user) identity;
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
  };
}
