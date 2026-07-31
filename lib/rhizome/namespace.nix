{ lib }:

/**
  Build the namespace tree for a set of discovered modules.

  The tree is shape, not identity: every module becomes an (empty) leaf
  and every directory an interior node, mirroring where the plugin's
  options mount. Consumers get an introspectable map of what a plugin
  ships without touching the fixpoint.

  Only the plugin itself is addressable as a string (see `plugin.nix`).
  Modules are reached by navigating from it — `self.programs.foo` to read
  a peer, `config.programs.foo` to write one.

  # Type

  ```
  buildNamespace :: [ { subpath : [String], file : Path } ] -> AttrSet
  ```
*/

modules:

let
  insert =
    tree: subpath:
    if subpath == [ ] then
      tree
    else
      tree
      // {
        ${lib.head subpath} = insert (tree.${lib.head subpath} or { }) (lib.tail subpath);
      };
in

lib.foldl' (tree: mod: insert tree mod.subpath) { } modules
