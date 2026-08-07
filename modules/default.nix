{ inputs, ... }:

let
  inherit (inputs) import-tree;
in

{
  imports = [
    (import-tree ./flake)
    (import-tree ./extensions)
    (import-tree ./profiles)
    (import-tree ./programs)
    (import-tree ./services)
    (import-tree ./system)
  ];
}
