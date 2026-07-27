# A second plugin's module. Nothing special: it exists to prove two
# namespaces mount side by side without colliding.
{ lib, ... }:
{
  options.marker = lib.mkOption {
    type = lib.types.str;
    default = "side";
  };
}
