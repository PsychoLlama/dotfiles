{ lib }:

# Planted into `home-manager.sharedModules` by an OS-level root guest.
# Marks the home-manager layer as already managed from above so the
# standalone home-manager edition can refuse to mount a second meta
# layer.
{
  options._meta.hosted = lib.mkOption {
    type = lib.types.bool;
    default = true;
    internal = true;
    description = "Whether an outer eval's meta layer already manages this configuration.";
  };
}
