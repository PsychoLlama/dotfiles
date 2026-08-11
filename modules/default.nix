{ inputs, ... }:

let
  inherit (inputs) import-tree;
in

{
  imports = [
    (import-tree ./flake)
    (import-tree ./rhizome)
    (import-tree ./editor/platform)
    (import-tree ./aspects/profiles)
  ];
}
