{ inputs, ... }:

let
  inherit (inputs) import-tree;
in

{
  imports = [
    (import-tree ./flake)
    (import-tree ./editor/platform)
    (import-tree ./profiles)
  ];
}
