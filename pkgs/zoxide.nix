let
  nixpkgs = import <nixpkgs> {};
in
  nixpkgs.rustPlatform.buildRustPackage rec {
    pname = "zoxide";
    version = "0.3.1";

    src = nixpkgs.fetchFromGitHub {
      owner = "ajeetdsouza";
      repo = "zoxide";
      rev = "v${version}";

      sha256 = "1sad18d0pxfdy9gvjmixzgdskg1l7djvzp0aipx7pz0lyi6gs23z";
    };

    cargoSha256 = "1sx3s1jnfxylbjr3x6v6j8a6zkl7hfyj4alzlyrsw36b1b64pwqm";

    meta = with nixpkgs.lib; {
      description = "A fast cd command that learns your habits";
      license = licenses.mit;
      maintainers = [ /* TODO: Add zoxide to nixpkgs. */ ];
    };
  }
