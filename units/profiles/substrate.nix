# What everything else stands on.
{
  rhizome,
  units,
  inputs,
  ...
}:

let
  inherit (inputs) agenix;
  inherit (units.platform) home-manager nixos;
in

rhizome.unit "profiles.substrate" (substrate: {
  main = rhizome.component [
    (nixos.module { imports = [ agenix.nixosModules.default ]; })
    (home-manager.module { imports = [ agenix.homeManagerModules.default ]; })
  ];

  unit = [
    substrate.main
    units.system.nix-daemon.unit
    units.system.package-set.unit
  ];
})
