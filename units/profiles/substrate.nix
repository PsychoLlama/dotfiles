# What everything else stands on.
{
  units,
  inputs,
  self,
  define,
  ...
}:

let
  inherit (inputs) agenix;
  inherit (units.platform) home-manager nixos;
in

{
  main = define.unit {
    config = [
      (nixos.module { imports = [ agenix.nixosModules.default ]; })
      (home-manager.module { imports = [ agenix.homeManagerModules.default ]; })
    ];
  };

  unit = define.group {
    config = [
      self.main
      units.system.nix-daemon.unit
      units.system.package-set.unit
    ];
  };
}
