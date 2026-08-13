{ inputs, lib, ... }:

let
  inherit (inputs) import-tree;

  inherit (import ./rhizome/_aspects.nix { inherit lib; }) import-aspect;
in

{
  imports = [
    (import-tree ./flake)
    (import-tree ./rhizome)
    (import-tree ./platform)
    ((import-tree.map (import-aspect ./aspects)) ./aspects)
  ];
}
