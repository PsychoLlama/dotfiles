flake-inputs:

let
  inherit (flake-inputs.nixpkgs) lib;
in

/*
  Recursively find all *.mod.nix files in a directory.
  Returns a list of paths suitable for use in module imports.

  Only files with the `.mod.nix` suffix are discovered, so plain `.nix`
  files can live alongside modules as helpers, data, or libraries without
  being imported into the module system.

  Type: { directory: Path, exclude?: [Path] } -> [Path]

  Example:
    discoverNixFiles { directory = ./platforms/nixos/modules; }
    => [ /path/to/platforms/nixos/modules/foo.mod.nix /path/to/platforms/nixos/modules/bar/baz.mod.nix ]

    discoverNixFiles {
      directory = ./platforms/editor/modules;
      exclude = [ ./platforms/editor/modules/psychollama ];
    }
    => [ /path/to/platforms/editor/modules/foo.mod.nix ] # excludes psychollama subdirectory
*/
{
  directory,
  exclude ? [ ],
}:

let
  allFiles = lib.filesystem.listFilesRecursive directory;

  # Filter for .mod.nix files (modules opt in via the suffix)
  nixFiles = lib.filter (path: lib.hasSuffix ".mod.nix" (toString path)) allFiles;

  # Convert exclude paths to strings for comparison
  excludeStrs = map toString exclude;

  # Filter out excluded paths
  excludedFiles = lib.filter (
    path:
    let
      pathStr = toString path;
      excluded = lib.any (
        excludePath:
        # Match directories (path contains excludePath/) or exact files
        lib.hasInfix (excludePath + "/") pathStr || pathStr == excludePath
      ) excludeStrs;
    in
    !excluded
  ) nixFiles;
in

excludedFiles
