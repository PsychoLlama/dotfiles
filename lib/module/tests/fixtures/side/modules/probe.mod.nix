# A second plugin's module. It exists to prove two namespaces mount side
# by side without colliding, and that `modules.root` targets whichever
# class the plugin happens to be mounted in.
{ cfg, lib, ... }:
{
  options.marker = lib.mkOption {
    type = lib.types.str;
    default = "side";
  };

  modules.root.aliasSetting = cfg.marker;
}
