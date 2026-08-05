{
  # Entity data, not platform config: aspects read it as `host.theme`, which
  # removes the need to bridge it from `osConfig` into home-manager.
  #
  # Declared through `imports` rather than `options` because `theme.palette`
  # derives its default from sibling options, which needs the entity's own
  # `config`.
  den.schema.host.imports = [ ./_theme.nix ];
}
