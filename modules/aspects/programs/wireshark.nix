{
  exports.nixos =
    { pkgs, ... }:

    {
      programs.wireshark = {
        enable = true;
        package = pkgs.unstable.wireshark;
      };
    };
}
