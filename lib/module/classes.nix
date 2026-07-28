# Built-in `modules.<block>` keys and the `_class` tag each one maps to.
# Plugins may extend this set (e.g. `classes.editor = "editor"`) but never
# remap an existing key. `root` is added by the root guest as an alias for
# whichever class it evaluates in, and is reserved.
{
  nixos = "nixos";
  darwin = "darwin";
  home-manager = "homeManager";
}
