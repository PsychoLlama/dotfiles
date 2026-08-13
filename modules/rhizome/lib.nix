{ inputs, lib, ... }:

let
  aspects = import ./_aspects.nix {
    inherit lib;
    inherit (inputs) import-tree;
  };
in

{
  flake.lib.rhizome = {
    inherit (aspects) import-aspects;
  };
}
