{ rhizome, units, ... }:

let
  inherit (units.platform) nixos;
in

rhizome.component "zram" [
  # Compressed. Defaults to 50% max of total RAM before paging to disk.
  # Competes with ARC for available memory.
  (nixos.module ({ zramSwap.enable = true; }))
]
