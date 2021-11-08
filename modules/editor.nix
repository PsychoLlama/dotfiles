{ config, unstable, inputs, lib, ... }:

let
  df = config.dotfiles;
  cfg = df.editor;

in {
  options = with lib; {
    dotfiles.editor = {
      enable = mkOption {
        type = types.bool;
        description = "Enable a heavily configured neovim editor";
        default = df.kitchen-sink.enable;
      };

      config = mkOption {
        type = types.path;
        description = "A vimrc file. Can be VimL or Lua.";
        default = ../config/neovim.lua;
      };

      linter.enable = mkOption {
        type = types.bool;
        description = "Enable a VimL linter";
        default = df.kitchen-sink.enable;
      };
    };
  };

  config = with lib;
    mkMerge [
      (mkIf cfg.enable {
        # Installed globally to play nicely with sudo.
        environment.systemPackages = [
          (unstable.nnn.override { withNerdIcons = true; })

          (unstable.neovim.override {
            configure.customRC = ''
              source ${cfg.config}
            '';

            configure.packages.plugins.start = with unstable.vimPlugins; [
              ale
              auto-pairs
              coc-nvim
              markdown-preview-nvim
              nnn-vim
              onedarkpro-nvim
              skim
              skim-vim
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
              teleport-vim
              vim-nand2tetris
              unison-vim

              # Nursery. Depends on the nursery's Nix overlay.
              git-vim
              misc-vim
              stacktrace-vim

              # Treesitter integrations.
              nvim-treesitter-textobjects
              (nvim-treesitter.withPlugins
                (plugins: unstable.tree-sitter.allGrammars))
            ];
          })
        ];

        environment.variables = {
          MANPAGER = "nvim -c 'setfiletype man' -";
          EDITOR = "nvim";
        };
      })

      (mkIf cfg.linter.enable {
        environment.systemPackages = [ unstable.vim-vint ];
      })
    ];
}
