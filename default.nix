with (import <nixpkgs> {});

callPackage ./pkgs/zsh-plugins {} // {
  inherit (gitAndTools) delta;
  inherit rustup shellcheck zsh travis tmux tmuxinator vim-vint yarn unzip;
  inherit glow ripgrep fd viu bat skim pastel hexyl zoxide procs dogdns;

  fnm = import ./pkgs/fast-node-manager/default.nix;
  neovim = neovim.override {
    configure = {
      customRC = builtins.readFile ./config/init.vim;
      packages.dotfiles.start =
        (callPackage ./pkgs/vim-plugins {})
        ++ (with vimPlugins; [coc-nvim])
        ++ [(callPackage ./editor {})];
    };
  };
}
