{ inputs, ... }:

let
  inherit (inputs) import-tree;
in

{
  imports = [
    (import-tree ./flake)
    (import-tree ./editor)
    (import-tree ./extensions)
    (import-tree ./profiles)
  ];
}
