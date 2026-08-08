{
  imports = [ ../system/identity.nix ];

  flake.modules.nixos.default =
    { config, lib, ... }:

    {
      networking.networkmanager.enable = lib.mkDefault true;

      # Lets the owner switch networks without sudo.
      users.users.${config.psychollama.identity.username}.extraGroups = [ "networkmanager" ];
    };
}
