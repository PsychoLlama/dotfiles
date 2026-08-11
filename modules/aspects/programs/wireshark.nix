{
  flake.modules.nixos.default =
    { pkgs, ... }:

    {
      programs.wireshark = {
        enable = true;
        package = pkgs.unstable.wireshark;
      };
    };
}
