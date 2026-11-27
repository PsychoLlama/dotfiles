# What everything else stands on.
{
  units,
  inputs,
  self,
  module,
  ...
}:

let
  inherit (inputs) agenix;
  inherit (units.platform) home-manager nixos;
in

{
  main = module.component "main" {
    config = [
      (nixos.module { imports = [ agenix.nixosModules.default ]; })
      (home-manager.module { imports = [ agenix.homeManagerModules.default ]; })
    ];
  };

  unit = module.group "unit" {
    config = [
      self.main
      units.system.nix-daemon.unit
      units.system.package-set.unit
    ];
  };
}
