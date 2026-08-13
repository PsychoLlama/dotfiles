{
  # Seeded so the substrate has a `platform` to mount for every class, whether
  # or not any extension defines one.
  flake.modules = {
    editor.platform = { };
    homeManager.platform = { };
    nixos.platform = { };
  };
}
