{
  # Gives the substrate a `platform` to mount for every class, extensions or not.
  flake = {
    editorModules.platform = { };
    homeModules.platform = { };
    nixosModules.platform = { };
  };
}
