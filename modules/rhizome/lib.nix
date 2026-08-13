{ inputs, lib, ... }:

let
  aspects = import ./_aspects.nix {
    inherit lib;
    inherit (inputs) import-tree;
  };
in

{
  /**
    The machinery behind this flake's module tree, exported so a consumer can
    grow one of its own rather than vendoring the loader.
  */
  flake.lib.rhizome = {
    inherit (aspects) import-aspects;
  };
}
