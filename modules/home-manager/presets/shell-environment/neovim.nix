{ config, lib, pkgs, ... }:

with lib;

let cfg = config.presets.neovim;

in {
  options.presets.neovim.enable =
    mkEnableOption "Configure Neovim as the one true editor";

  config = mkIf cfg.enable {
    home.sessionVariables.MANPAGER =
      "${config.programs.neovim.finalPackage}/bin/nvim -c 'Man!'";

    programs.neovim = {
      enable = true;
      coc.enable = true;

      package = recursiveUpdate pkgs.unstable.neovim-unwrapped {
        # Used by home-manager, which still assumes nvim 0.7.
        lua.pkgs.lib = pkgs.unstable.neovim-unwrapped.lua.pkgs.luaLib;
      };

      plugins = with pkgs.vimPlugins; [
        ale
        auto-pairs
        coc-json
        coc-rls
        coc-tsserver
        fzf-vim
        markdown-preview-nvim
        onedarkpro-nvim
        splitjoin-vim
        undotree
        vader-vim
        vim-commentary
        vim-endwise
        vim-fugitive
        vim-gitgutter
        vim-graphql
        vim-plug
        vim-repeat
        vim-surround
        vim-terraform

        # 3rd party
        alternaut-vim
        further-vim
        navitron-nvim
        teleport-vim
        unison-vim
        personal-vim-config

        # Treesitter integrations.
        nvim-treesitter-textobjects
        (nvim-treesitter.withPlugins (plugins: attrValues plugins))
      ];

      extraPackages = with pkgs.unstable; [ vim-vint nixfmt shellcheck nodejs ];

      extraConfig = ''
        set shell=${pkgs.zsh}/bin/zsh
        source ${../../../../config/neovim.lua}
      '';
    };
  };
}
