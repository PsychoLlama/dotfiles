{ config, lib, ... }:

let
  aspects = import ./_aspects.nix { inherit lib; };
in

{
  imports = [ ./aspects.nix ];

  flake.lib.rhizome = {
    inherit (aspects) import-aspect;

    load-modules = aspects.mkLoadModules {
      root = ../aspects;
      aspects = config.rhizome.aspects;
      modules = config.flake.modules;
    };
  };
}
