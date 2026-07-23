{ lib }:

/*
  Build the handle tree for a set of discovered modules.

  Every module becomes a leaf whose `__toString` is the file's absolute
  path — the "handle". A handle works as a dynamic attribute name
  (`config."${self.theme}"`), and because the path is unique per store
  copy, handles from different plugins can never collide. Interior nodes
  are plain attrsets; a directory module keeps its children as siblings
  of `__toString`.

  Type: [ { subpath : [String], file : Path } ] -> AttrSet
*/

modules:

let
  insert =
    tree: subpath: file:
    if subpath == [ ] then
      tree // { __toString = _: toString file; }
    else
      tree
      // {
        ${lib.head subpath} = insert (tree.${lib.head subpath} or { }) (lib.tail subpath) file;
      };
in

lib.foldl' (tree: mod: insert tree mod.subpath mod.file) { } modules
