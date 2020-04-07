with (import <nixpkgs> {});

{
  inherit rustup shellcheck zsh travis tmux tmuxinator vim-vint yarn;
  inherit unzip glow antigen neovim ripgrep fd viu bat skim pastel;
  inherit hexyl;

  git-delta = import ./pkgs/git-delta.nix;
  zoxide = import ./pkgs/zoxide.nix;
}
