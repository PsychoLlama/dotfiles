{
  units,
  inputs,
  define,
  ...
}:

let
  inherit (inputs) nixpkgs-unstable;
  overlays = inputs.self.overlays;
in

let
  inherit (units.platform) nixos;
in

{
  unit = define.unit {
    config = [
      (nixos.module (
        { lib, ... }:

        {
          nixpkgs = {
            overlays = lib.attrValues overlays;

            config = {
              # Unfree licenses. To be replaced with libre alternatives.
              allowUnfreePredicate =
                pkg:
                lib.elem (lib.getName pkg) [
                  "claude-code-bin"
                  "spotify"
                ];
            };

            # Pin `<nixpkgs>` and `flake:nixpkgs` to match system packages.
            flake = {
              source = lib.mkForce nixpkgs-unstable; # Stable is dumb. Live a little.
              setNixPath = lib.mkForce true;
              setFlakeRegistry = true;
            };
          };
        }
      ))
    ];
  };
}
