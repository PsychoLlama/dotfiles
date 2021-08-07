# Overlays custom software onto nixpkgs.
self: super: {
  slock = super.slock.overrideAttrs (old: {
    patches = (old.patches or [ ]) ++ [ ../config/slock-theme.patch ];
  });

  # TODO: Add the other packages.
}
