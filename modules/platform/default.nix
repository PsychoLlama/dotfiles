{
  # Gives the substrate a `platform` to mount for every class, extensions or not.
  flake = {
    homeModules.platform = { };
    nixosModules.platform = { };
  };
}
