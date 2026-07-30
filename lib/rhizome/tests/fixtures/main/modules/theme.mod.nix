{ cfg, lib, ... }:
{
  options = {
    name = lib.mkOption {
      type = lib.types.str;
      default = "plain";
    };

    # A computed export: readOnly, derived from the module's own config.
    palette = lib.mkOption {
      type = lib.types.attrsOf lib.types.str;
      readOnly = true;
      default = (import ./palettes.nix).${cfg.name};
    };
  };
}
