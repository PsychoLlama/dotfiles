# Overlays custom software onto nixpkgs.
self: super: {
  nnn-full = super.callPackage ./nnn.nix { };
}
