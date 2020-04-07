with builtins; let
  nixpkgs = import <nixpkgs> {};
  os = substring (stringLength currentSystem - 6) (-1) currentSystem;
  release =
    if os == "darwin"
    then {
      filename = "fnm-macos";
      hash = "1bd5xb352q66kcci1yld7qb504f6c2dqja6dllwmxlscbmb864r9";
    } else {
      filename = "fnm-linux";
      hash = "14f8i1qqnk6fk48wqhv4f7lpnfz53jcpc72ka0l7f3iig3biss83";
    };
in

derivation rec {
  name = "fnm";
  system = builtins.currentSystem;
  version = "1.20.0";

  builder = "${nixpkgs.bash}/bin/bash";
  args = [ ./builder.bash ];
  setup = ./setup.bash;
  buildInputs = [ nixpkgs.coreutils ];

  src = fetchTarball {
    url = "https://github.com/Schniz/fnm/releases/download/v${version}/${release.filename}.zip";
    sha256 = release.hash;
  };
}
