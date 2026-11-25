# Loads this directory as one attrset that mirrors it: `programs/bat.nix` is
# `units.programs.bat`.
#
# A directory's `default.nix` holds what belongs to the directory itself. When
# that is a single unit, it is the directory's `unit`: `programs/tmux/default.nix`
# is `units.programs.tmux.unit`. When it is an attrset of named things, they
# sit beside the directory's files. Tests (`*.test.nix`) sit beside what they cover and are skipped.
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
          else if
            name == "default.nix"
            || lib.hasPrefix "_" name
            || lib.hasSuffix ".test.nix" name
            || !lib.hasSuffix ".nix" name
          then
            { }
          else
            { ${lib.removeSuffix ".nix" name} = import (directory + "/${name}") arguments; }
        ) entries;

        own = import (directory + "/default.nix") arguments;
        named = lib.isAttrs own && !(own ? kind);
      in
      if !(entries ? "default.nix") || directory == ./. then
        contents
      else if named then
        contents // own
      else
        contents // { unit = own; };
  in
  load ./.
)
