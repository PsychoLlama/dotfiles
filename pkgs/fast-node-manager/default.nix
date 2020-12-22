with builtins; let
  nixpkgs = import <nixpkgs> {};
  os = substring (stringLength currentSystem - 6) (-1) currentSystem;
  release =
    if os == "darwin" then {
      zipfile = "fnm-macos";
      hash = "0k0fwhsna6fgri1hiznw6bhii56g48bp7x19z1wskr9iknnha2sb";
    } else {
      zipfile = "fnm-linux";
      hash = "0j9anh3jcng1xc4zdmy9j07cdssl0bwi9nsz489qdzyd11y12i97";
    };
in

derivation rec {
  name = "fnm";
  system = builtins.currentSystem;
  version = "1.22.8";

  builder = "${nixpkgs.bash}/bin/bash";
  args = [ ./builder.bash ];
  setup = ./setup.bash;
  buildInputs = [ nixpkgs.coreutils nixpkgs.unzip ];

  src = builtins.fetchurl {
    url = "https://github.com/Schniz/fnm/releases/download/v${version}/${release.zipfile}.zip";
    sha256 = release.hash;
  };
}
