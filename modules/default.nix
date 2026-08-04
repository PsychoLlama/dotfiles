{ inputs, ... }:

{
  imports = [ (inputs.import-tree ./flake) ];
}
