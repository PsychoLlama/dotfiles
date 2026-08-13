{ lib, ... }:

{
  options.rhizome.aspects = lib.mkOption {
    description = "Aspects published by the sweep, keyed by id.";
    internal = true;
    default = { };

    type = lib.types.attrsOf (
      lib.types.submodule {
        options.dependencies = lib.mkOption {
          description = "Ids of the aspects this one imports.";
          type = lib.types.listOf lib.types.str;
          default = [ ];
        };
      }
    );
  };
}
