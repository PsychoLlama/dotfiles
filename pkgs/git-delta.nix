let
  nixpkgs = import <nixpkgs> {};
in
  nixpkgs.rustPlatform.buildRustPackage rec {
    pname = "delta";
    version = "0.0.17";

    src = nixpkgs.fetchFromGitHub {
      owner = "dandavison";
      repo = "delta";
      rev = version;

      sha256 = "1j01h60snciqp4psyxf67j3gbmi02c1baprsg9frzjacawbx8cz7";
    };

    cargoSha256 = "176bfd57gc9casvk0p10ilvzw3q3rkkv7qflja778vrwr9zrmkzq";

    meta = with nixpkgs.lib; {
      licenses = licenses.mit;
      description = "A syntax-highlighter for git and diff output";
      maintainers = [ /* TODO: Add git-delta to nixpkgs. */ ];
    };
  }
