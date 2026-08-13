{ inputs, ... }:

let
  inherit (inputs) agenix;
in

{
  flake.nixosModules.secrets = agenix.nixosModules.default;
  flake.homeModules.secrets = agenix.homeManagerModules.default;
}
