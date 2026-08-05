{
  # Temporary: the palette now lives on the host entity. Legacy presets still
  # read `config.theme`, so the same declaration is imported into the platform
  # until they migrate. Deleted once nothing outside aspects reads it.
  imports = [ ../../den/schema/_theme.nix ];
}
