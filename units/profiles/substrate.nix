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

[
  (rhizome.component "profiles.substrate" [

    (nixos.module ({
      imports = [
        nixosModules.platform
        agenix.nixosModules.default
      ];
    }))

    (home-manager.module ({ imports = [ agenix.homeManagerModules.default ]; }))
  ])
  units.system.home-manager
  units.system.nix-daemon
  units.system.package-set
]
