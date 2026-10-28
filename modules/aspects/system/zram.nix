{
  # Compressed. Defaults to 50% max of total RAM before paging to disk.
  # Competes with ARC for available memory.
  exports.nixos.zramSwap.enable = true;
}
