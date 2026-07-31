{ lib }:

/**
  Recursively find rhizome module files under a source directory.

  `foo.mod.nix` mounts at `foo`; `foo/mod.nix` mounts at `foo`. Plain
  `.nix` files are helpers and never discovered. A file and a directory
  competing for the same mount point is an error.

  # Type

  ```
  discover :: Path -> [ { subpath : [String], file : Path } ]
  ```
*/

src:

let
  walk =
    subpath: directory:
    lib.pipe (lib.readDir directory) [
      (lib.mapAttrsToList (
        name: kind:
        if kind == "directory" then
          walk (subpath ++ [ name ]) (directory + "/${name}")
        else if name == "mod.nix" then
          if subpath == [ ] then
            throw "rhizome: `mod.nix` cannot sit at the top of ${lib.toString src}. That mount point is reserved for the plugin's own node."
          else
            [
              {
                inherit subpath;
                file = directory + "/${name}";
              }
            ]
        else if lib.hasSuffix ".mod.nix" name then
          [
            {
              subpath = subpath ++ [ (lib.removeSuffix ".mod.nix" name) ];
              file = directory + "/${name}";
            }
          ]
        else
          [ ]
      ))
      lib.concatLists
    ];

  modules = walk [ ] src;

  duplicates = lib.pipe modules [
    (lib.groupBy (mod: lib.concatStringsSep "." mod.subpath))
    (lib.filterAttrs (_: entries: lib.length entries > 1))
    lib.attrNames
  ];
in

if duplicates == [ ] then
  modules
else
  throw "rhizome: multiple files mount at `${lib.head duplicates}` in ${lib.toString src}. Use either `<name>.mod.nix` or `<name>/mod.nix`, not both."
