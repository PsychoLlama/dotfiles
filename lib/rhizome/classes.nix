# Built-in `modules.<block>` keys and the `_class` tag each one maps to.
# Plugins may extend this set (e.g. `classes.editor = "editor"`) but never
# remap an existing key. Every block names a class outright — a module
# always says which host it is configuring.
{
  nixos = "nixos";
  darwin = "darwin";
  home-manager = "homeManager";
}
