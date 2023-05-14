{
  description = "Development environment";

  inputs.rust-overlay.url = "github:oxalica/rust-overlay";

  outputs = { self, nixpkgs, rust-overlay }:
    let inherit (nixpkgs) lib;

    in {
      devShell = lib.genAttrs lib.systems.flakeExposed (system:
        let
          overlays = [ (import rust-overlay) ];
          pkgs = import nixpkgs { inherit system overlays; };

        in pkgs.mkShell {
          nativeBuildInputs = with pkgs; [ rust-bin.stable.latest.complete ];
        });
    };
}
