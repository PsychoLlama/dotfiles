{
  description = "Development environment";

  inputs = {
    rust-overlay.url = "github:oxalica/rust-overlay";
    systems.url = "github:nix-systems/default";
  };

  outputs =
    {
      self,
      nixpkgs,
      rust-overlay,
      systems,
    }:

    let
      inherit (nixpkgs) lib;

      overlays = [ (import rust-overlay) ];

      eachSystem = lib.flip lib.mapAttrs (
        lib.genAttrs (import systems) (
          system:
          import nixpkgs {
            inherit system overlays;
          }
        )
      );
    in

    {
      devShell = eachSystem (
        system: pkgs:
        pkgs.mkShell {
          packages = [
            (pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml)
          ];
        }
      );
    };
}
