with (import <nixpkgs> {});

callPackage ./pkgs/zsh-plugins {} // {
  inherit (gitAndTools) delta;
  inherit rustup cargo-edit shellcheck zsh tmux tmuxinator vim-vint yarn;
  inherit unzip glow ripgrep fd viu bat skim pastel hexyl zoxide procs;
  inherit dogdns jq miniserve zathura;

  fnm = import ./pkgs/fast-node-manager/default.nix;
  neovim = neovim.override {
    configure = {
      customRC = builtins.readFile ./config/init.vim;
      packages.plugins.start =
        (callPackage ./pkgs/vim-plugins {})
        ++ [vimPlugins.coc-nvim];

      packages.nursery.start = callPackage ./editor {};
    };
  };
}
