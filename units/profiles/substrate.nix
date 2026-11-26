{
  rhizome,
  units,
  inputs,
  ...
}:

let
  inherit (inputs) agenix;
  inherit (inputs.self) nixosModules;
in

let
  inherit (units.platform) home-manager nixos;
in

rhizome.unit "profiles.substrate" (substrate: {
  main = rhizome.component [

    (nixos.module ({
      imports = [
        nixosModules.platform
        agenix.nixosModules.default
      ];
    }))

    (home-manager.module ({ imports = [ agenix.homeManagerModules.default ]; }))
  ];

  unit = [
    substrate.main
    units.system.home-manager.unit
    units.system.nix-daemon.unit
    units.system.package-set.unit
  ];
})
