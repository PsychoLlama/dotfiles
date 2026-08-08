{
  # The public menu. `modules/default.nix` separately imports every profile,
  # which is what applies them to this flake's own hosts; downstream picks from
  # here instead, one profile at a time.
  #
  # Profiles are flake-parts modules, so they can't ride on `flake.modules`:
  # that option stamps `_class` from its attribute name, and anything but
  # `flake` would be rejected by the consumer's evaluation.
  flake.profiles = {
    full = ../profiles/full.nix;
    home-lab-admin = ../profiles/home-lab-admin.nix;
    linux-desktop = ../profiles/linux-desktop.nix;
  };
}
