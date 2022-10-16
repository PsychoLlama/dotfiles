{ config, pkgs, nixpkgs-unstable, inputs, lib, ... }:

let
  df = config.dotfiles;
  cfg = df.editor;

  # Non-standard vim plugins.
  extraVimPlugins = with lib;
    mapAttrs (pluginName: plugin:
      pkgs.vimUtils.buildVimPluginFrom2Nix {
        pname = pluginName;
        version = plugin.shortRev or "latest";
        src = plugin;
      }) {
        inherit (inputs) further-vim teleport-vim alternaut-vim navitron-nvim;
        unison-vim = "${inputs.unison-vim}/editor-support/vim";
        personal-vim-config = ../config/editor;
      };

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

      extraConfig = mkOption {
        type = types.lines;
        description = "Extra lines to append to vimrc";
        default = "";
      };
    };
  };

  config = with lib;
    mkMerge [
      (mkIf cfg.enable {
        # Installed globally to play nicely with sudo.
        environment.systemPackages = [
          (nixpkgs-unstable.neovim.override {
            configure.customRC = ''
              source ${cfg.config}
              ${cfg.extraConfig}
            '';

            configure.packages.plugins.start =
              with nixpkgs-unstable.vimPlugins // extraVimPlugins; [
                ale
                auto-pairs
                coc-nvim
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
                (nvim-treesitter.withPlugins
                  (plugins: nixpkgs-unstable.tree-sitter.allGrammars))
              ];
          })
        ];

        environment.variables = {
          MANPAGER = "nvim -c 'Man!'";
          EDITOR = "nvim";
        };
      })

      (mkIf cfg.linter.enable {
        environment.systemPackages = [ nixpkgs-unstable.vim-vint ];
      })
    ];
}
