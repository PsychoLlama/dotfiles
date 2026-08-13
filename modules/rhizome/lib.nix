{ lib, ... }:

let
  aspects = import ./_aspects.nix { inherit lib; };
in

{
  flake.lib.rhizome = {
    inherit (aspects) import-aspect;
  };
}
