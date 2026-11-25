# Loads this directory as one attrset that mirrors it: `programs/bat.nix` is
# `units.programs.bat`. A directory's `default.nix` holds what belongs to the
# directory itself. Tests (`*.test.nix`) sit beside what they cover and are skipped.
{
  lib,
  rhizome,
  inputs,
}:

lib.fix (
  units:
  let
    arguments = {
      inherit
        lib
        rhizome
        inputs
        units
        ;
    };

    load =
      directory:
      let
        entries = builtins.readDir directory;

        contents = lib.concatMapAttrs (
          name: type:
          if type == "directory" then
            { ${name} = load (directory + "/${name}"); }
          else if name == "default.nix" || lib.hasSuffix ".test.nix" name || !lib.hasSuffix ".nix" name then
            { }
          else
            { ${lib.removeSuffix ".nix" name} = import (directory + "/${name}") arguments; }
        ) entries;

        own =
          if entries ? "default.nix" && directory != ./. then
            import (directory + "/default.nix") arguments
          else
            { };
      in
      contents // own;
  in
  load ./.
)
