# The part of `ava` that has moved here. The rest is still in
# `modules/nodes/ava`, which imports what this produces.
{ units, ... }:

let
  inherit (units.platform) home-manager nixos;
  inherit (units) programs;
in

{ username }:

nixos.host [ (home-manager.user { name = username; } [ programs.bat ]) ]
