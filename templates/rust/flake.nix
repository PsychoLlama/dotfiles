{
  description = "Development environment";

  inputs.rust-overlay.url = "github:oxalica/rust-overlay";

  outputs = { self, nixpkgs, rust-overlay }:
    let
      inherit (nixpkgs) lib;

      overlays = [ (import rust-overlay) ];

      systems =
        [ "aarch64-linux" "aarch64-darwin" "x86_64-darwin" "x86_64-linux" ];

      eachSystem = lib.flip lib.mapAttrs (lib.genAttrs systems
        (system: import nixpkgs { inherit system overlays; }));

    in {
      devShell = eachSystem (system: pkgs:
        pkgs.mkShell {
          nativeBuildInputs =
            [ (pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml) ];
        });
    };
}
