{
  description = "Development environment";

  outputs = { self, nixpkgs }:
    let inherit (nixpkgs) lib;

    in {
      devShell = lib.genAttrs lib.systems.flakeExposed (system:
        let pkgs = import nixpkgs { inherit system; };

        in pkgs.mkShell {
          nativeBuildInputs = with pkgs; [ cargo clippy rustc rustfmt ];
        });
    };
}
