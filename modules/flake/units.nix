{ inputs, lib, ... }:

{
  _module.args.units = import ../../units {
    inherit lib inputs;
    rhizome = inputs.rhizome.lib;
  };
}
