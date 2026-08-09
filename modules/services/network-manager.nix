{ config, ... }:

# Bound out here because the module below shadows `config` with its own.
let
  inherit (config.identity) username;
in

{
  imports = [ ../system/identity.nix ];

  flake.modules.nixos.default =
    { config, lib, ... }:

    {
      networking.networkmanager.enable = lib.mkDefault true;

      # Lets the owner switch networks without sudo.
      users.users.${username}.extraGroups = [ "networkmanager" ];
    };
}
