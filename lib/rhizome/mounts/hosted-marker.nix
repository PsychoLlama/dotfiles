{ lib }:

# Planted into `home-manager.sharedModules` by an OS-level mount. Marks
# the home-manager layer as already managed from above so the standalone
# home-manager edition can refuse to install a second rhizome layer.
{
  options.rhizome.hosted = lib.mkOption {
    type = lib.types.bool;
    default = true;
    internal = true;
    description = "Whether an outer eval's rhizome layer already manages this configuration.";
  };
}
