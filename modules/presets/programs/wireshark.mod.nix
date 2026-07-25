{ pkgs, ... }:

{
  platforms.nixos.programs.wireshark = {
    enable = true;
    package = pkgs.unstable.wireshark;
  };
}
