# Upgrade a number of packages to their bleeding edge versions.
#
# Signature: inputs.nixpkgs-unstable => overlay
nixpkgs-unstable: self: pkgs:

let latest = nixpkgs-unstable.legacyPackages.${pkgs.system};

in {
  inherit (latest)
    rofi dunst alacritty zoxide starship tmux fira-code nerdfonts exa bat
    vim-vint fd bottom ncspot nixfmt shellcheck;

  neovim-unwrapped = pkgs.lib.recursiveUpdate latest.neovim-unwrapped {
    # Used by home-manager, which still assumes nvim 0.7.
    lua.pkgs.lib = latest.neovim-unwrapped.lua.pkgs.luaLib;
  };

  vimPlugins = pkgs.vimPlugins // {
    inherit (latest.vimPlugins)
      nvim-treesitter-textobjects nvim-treesitter ale fzf-vim
      markdown-preview-nvim coc-nvim coc-json coc-tsserver coc-rls
      onedarkpro-nvim;
  };
}
