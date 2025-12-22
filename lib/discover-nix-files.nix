flake-inputs:

let
  inherit (flake-inputs.nixpkgs) lib;
in

/*
  Recursively find all .nix files in a directory.
  Returns a list of paths suitable for use in module imports.

  Type: { directory: Path, exclude?: [Path] } -> [Path]

  Example:
    discoverNixFiles { directory = ./platforms/nixos/modules; }
    => [ /path/to/platforms/nixos/modules/foo.nix /path/to/platforms/nixos/modules/bar/baz.nix ]

    discoverNixFiles {
      directory = ./platforms/editor/modules;
      exclude = [ ./platforms/editor/modules/psychollama ];
    }
    => [ /path/to/platforms/editor/modules/foo.nix ] # excludes psychollama subdirectory
*/
{
  directory,
  exclude ? [ ],
}:

let
  allFiles = lib.filesystem.listFilesRecursive directory;

  # Filter for .nix files
  nixFiles = lib.filter (path: lib.hasSuffix ".nix" (toString path)) allFiles;

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
