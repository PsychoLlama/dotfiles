{ inputs, ... }:

let
  inherit (inputs) import-tree;
in

{
  imports = [
    (import-tree ./flake)
    (import-tree ./extensions)
    (import-tree ./programs)
    (import-tree ./services)
  ];
}
