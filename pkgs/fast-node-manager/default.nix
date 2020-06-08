with builtins; let
  nixpkgs = import <nixpkgs> {};
  os = substring (stringLength currentSystem - 6) (-1) currentSystem;
  release =
    if os == "darwin" then {
      zipfile = "fnm-macos";
      hash = "1wzsa7hx6da2wfdw4fnqnbi14khjylyhdaz9j05l8pp77cmvwhj3";
    } else {
      zipfile = "fnm-linux";
      hash = "16x091g0bh3dspxy9x8ypbx4zx79jw9ks9z8a6bj78y0v395ycb1";
    };
in

derivation rec {
  name = "fnm";
  system = builtins.currentSystem;
  version = "1.21.0";

  builder = "${nixpkgs.bash}/bin/bash";
  args = [ ./builder.bash ];
  setup = ./setup.bash;
  buildInputs = [ nixpkgs.coreutils nixpkgs.unzip ];
  zipDirectoryName = release.zipfile;

  src = builtins.fetchurl {
    url = "https://github.com/Schniz/fnm/releases/download/v${version}/${release.zipfile}.zip";
    sha256 = release.hash;
  };
}
