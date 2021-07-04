let overlay = self: super: {
  slock = super.slock.overrideAttrs(old: {
    patches = (old.patches or []) ++ [
      ./config/slock-theme.patch
    ];
  });
};

in [overlay]
