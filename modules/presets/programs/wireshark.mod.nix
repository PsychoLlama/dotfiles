{ pkgs, ... }:

{
  modules.nixos.programs.wireshark = {
    enable = true;
    package = pkgs.unstable.wireshark;
  };
}
