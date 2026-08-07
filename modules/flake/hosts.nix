{ config, inputs, ... }:

{
  flake.nixosConfigurations = config.flake.lib.hosts.nixos {
    ava = [
      inputs.nixos-hardware.nixosModules.lenovo-thinkpad-p1-gen3
      inputs.nixpkgs.nixosModules.notDetected
      ../../hosts/ava
    ];
  };
}
