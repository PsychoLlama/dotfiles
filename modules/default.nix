{ inputs, ... }:

let
  inherit (inputs) import-tree;
in

{
  imports = [
    (import-tree ./flake)
    (import-tree ./rhizome)
    (import-tree ./platform)
    (import-tree ./aspects/profiles)
  ];
}
