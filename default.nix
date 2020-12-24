with (import <nixpkgs> {});

{
  inherit rustup shellcheck zsh travis tmux tmuxinator vim-vint yarn;
  inherit unzip glow antigen ripgrep fd viu bat skim pastel hexyl;
  inherit zoxide procs dogdns;

  inherit (gitAndTools) delta;

  fnm = import ./pkgs/fast-node-manager/default.nix;

  neovim = neovim.override {
    configure = {
      customRC = builtins.readFile ./linked/init.vim;
      packages.dotfiles.start =
        (callPackage ./pkgs/vim-plugins/vim-plugins.nix {})
        ++ (with vimPlugins; [coc-nvim])
        ++ [(callPackage ./editor {})];
    };
  };
}
