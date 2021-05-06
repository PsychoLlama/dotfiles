config@{
  pkgs ? import <nixpkgs> config,
  vimPlugins ? import <vim-plugins> config,
}:

with pkgs;

callPackage ./pkgs/zsh-plugins config // {
  inherit (gitAndTools) delta;
  inherit rustup cargo-edit shellcheck zsh tmux tmuxinator vim-vint yarn;
  inherit unzip glow ripgrep fd viu bat skim pastel hexyl zoxide dogdns;
  inherit jq miniserve pv zathura ipfs;

  fnm = import ./pkgs/fast-node-manager/default.nix;
  neovim = neovim.override {
    configure = {
      customRC = builtins.readFile ./config/init.vim;

      packages.plugins.start = with vimPlugins; [
        # Common
        coc-nvim
        ale
        auto-pairs
        godown-vim
        nginx-vim
        onedark-vim
        rust-vim
        skim
        skim-vim
        splitjoin-vim
        typescript-vim
        undotree
        vader-vim
        vim-commentary
        vim-endwise
        vim-fugitive
        vim-gitgutter
        vim-graphql
        vim-jsx
        vim-markdown
        vim-nand2tetris-syntax
        vim-nix
        vim-repeat
        vim-surround
        vim-swap
        vim-terraform
        vim-toml
        yajs-vim

        # Home-grown
        alternaut-vim
        further-vim
        teleport-vim
        navitron-vim

        # Plugin Nursery
        clippy-nvim
        git-vim
        misc-vim
        stacktrace-vim
      ];
    };
  };
}
