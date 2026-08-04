{ inputs, ... }:

{
  imports = [ ./common.nix ];

  den.aspects.ava.nixos.imports = [
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-p1-gen3
    inputs.nixpkgs.nixosModules.notDetected
    ./ava
  ];

  den.hosts.x86_64-linux.ava = { };
}
