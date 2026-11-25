{ inputs, lib, ... }:

let
  rhizome = inputs.rhizome.lib;
  units = import ../../units { inherit lib inputs rhizome; };

  trees = lib.mapAttrs (_: host: rhizome.eval { } host.unit) units.hosts;
in

{
  _module.args.units = units;

  flake = {
    # One machine per tree in `units/hosts`.
    nixosConfigurations = lib.mapAttrs (
      _: tree:
      inputs.nixpkgs.lib.nixosSystem {
        modules = tree.collect units.platform.nixos.modules;
      }
    ) trees;

    # Who writes what to whom, as Graphviz. See `just graph`.
    graphs = lib.mapAttrs (_: tree: rhizome.graph.dot tree.routes) trees;
  };
}
