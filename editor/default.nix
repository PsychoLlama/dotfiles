{ pkgs ? import <nixpkgs> {} }:

pkgs.vimUtils.buildVimPluginFrom2Nix {
  pname = "vim-dotfiles";
  version = "latest";
  src = {
    type = "derivation";
    name = "dotfiles";
    outPath = ./.;
  };
}
