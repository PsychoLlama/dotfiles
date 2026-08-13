{ inputs, lib, ... }:

let
  inherit (inputs) import-tree;

  inherit (import ./rhizome/_aspects.nix { inherit lib import-tree; }) import-aspects;
in

{
  imports = [
    (import-tree ./flake)
    (import-tree ./rhizome)
    (import-tree ./platform)
    (import-aspects ./aspects { })
    (import-tree ./hosts)
  ];
}
