{ units, module, ... }:

let
  inherit (units.platform) nixos;
in

{
  unit = module.component {
    config = [
      # Compressed. Defaults to 50% max of total RAM before paging to disk.
      # Competes with ARC for available memory.
      (nixos.module ({ zramSwap.enable = true; }))
    ];
  };
}
