{
  exports.nixos =
    { config, lib, ... }:

    {
      services.avahi = {
        enable = lib.mkDefault true;
        nssmdns4 = lib.mkDefault true;
      };
    };
}
