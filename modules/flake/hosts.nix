{ config, inputs, ... }:

{
  den.aspects.ava.nixos.imports = [
    inputs.nixos-hardware.nixosModules.lenovo-thinkpad-p1-gen3
    inputs.nixpkgs.nixosModules.notDetected
    ../../hosts/ava
  ];

  den.hosts.x86_64-linux.ava.instantiate = config.flake.lib.hosts.nixos "ava";
}
