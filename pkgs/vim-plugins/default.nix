{ pkgs ? import <nixpkgs> {} }:

# Everything necessary to run ./update-plugins.js and generate the lockfile.
pkgs.mkShell {
  nativeBuildInputs = with pkgs; [
    nix-prefetch-git
    nodejs
  ];
}
