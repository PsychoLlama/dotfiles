{ config, unstable, lib, ... }:

let cfg = config.dotfiles.editor;

in {
  options = with lib; {
    dotfiles.editor = {
      enable = mkOption {
        type = types.bool;
        description = "Enable a heavily configured neovim editor";
        default = true;
      };

      config = mkOption {
        type = types.path;
        description = "A vimrc file. Can be VimL or Lua.";
        default = ../config/neovim.lua;
      };

      linter.enable = mkOption {
        type = types.bool;
        description = "Enable a VimL linter";
        default = true;
      };
    };
  };

  config = with lib; {
    # Installed globally to play nicely with sudo.
    environment.systemPackages = mkIf cfg.enable ([
      (unstable.neovim.override {
        configure.customRC = ''
          source ${cfg.config}
        '';

        configure.packages.plugins.start = with unstable.vimPlugins; [
          ale
          auto-pairs
          coc-nvim
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
          vim-markdown
          vim-nix
          vim-plug
          vim-repeat
          vim-surround
          vim-swap
          vim-terraform
          vim-toml

          # 3rd party
          alternaut-vim
          further-vim
          godown-vim
          navitron-vim
          nginx-vim
          teleport-vim
          vim-nand2tetris
          yajs-vim

          # Nursery. Depends on the nursery's Nix overlay.
          clippy-nvim
          git-vim
          misc-vim
          stacktrace-vim
        ];
      })
    ] ++ (if cfg.linter.enable then [ unstable.vim-vint ] else [ ]));

    environment.variables = mkIf cfg.enable {
      MANPAGER = "nvim -c 'setfiletype man' -";
      EDITOR = "nvim";
    };
  };
}
