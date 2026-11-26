{ inputs, lib, ... }:

let
  rhizome = inputs.rhizome.lib;
  units = rhizome.load ../../units { inherit inputs lib; };

  trees = lib.mapAttrs (_: host: rhizome.eval { } host.unit) units.hosts;
in

{
  _module.args.units = units;

  flake = {
    # One machine per tree in `units/hosts`.
    nixosConfigurations = lib.mapAttrs (
      _: tree:
      inputs.nixpkgs.lib.nixosSystem {
        modules = lib.concatMap (machine: machine.modules) (tree.collect units.platform.nixos.machine);
      }
    ) trees;

    # Who writes what to whom, as Graphviz. See `just graph`.
    graphs = lib.mapAttrs (_: tree: rhizome.graph.dot tree.routes) trees;
  };
}
