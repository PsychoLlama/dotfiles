{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.presets.neovim;
  yaml = pkgs.formats.yaml { };
  efm-config = {
    languages = rec {
      vim = {
        lint-command = "vint -";
        lint-source = "vint";
        lint-stdin = true;
        lint-formats = [ "%f:%l:%c: %m" ];
      };

      bash = {
        lint-command = "shellcheck -f gcc -x -";
        lint-source = "shellcheck";
        lint-stdin = true;
        lint-formats = [
          "%f:%l:%c: %trror: %m"
          "%f:%l:%c: %tarning: %m"
          "%f:%l:%c: %tote: %m"
        ];
      };

      sh = bash;

      nix = {
        format-command = "nixfmt --quiet";
        format-stdin = true;
      };
    };
  };

in {
  options.presets.neovim.enable =
    mkEnableOption "Configure Neovim as the one true editor";

  config = mkIf cfg.enable {
    home.sessionVariables = let inherit (config.programs.neovim) finalPackage;
    in {
      EDITOR = "${finalPackage}/bin/nvim";
      MANPAGER = "${finalPackage}/bin/nvim -c 'Man!'";
    };

    programs.neovim = {
      enable = true;
      coc = {
        enable = true;

        settings = with pkgs.unstable; {
          languageserver = {
            rust = {
              command = "${rust-analyzer}/bin/rust-analyzer";
              filetypes = [ "rust" ];
              rootPatterns = [ "Cargo.toml" ];
            };

            terraform = {
              command = "${terraform-ls}/bin/terraform-ls";
              args = [ "serve" ];
              filetypes = [ "terraform" "hcl" ];
            };

            efm = {
              command = "${efm-langserver}/bin/efm-langserver";
              args = [ "-c" (yaml.generate "efm-config.yaml" efm-config) ];
              filetypes = attrNames efm-config.languages;
            };
          };

          "eslint.autoFixOnSave" = true;
          "tsserver.useLocalTsdk" = true;
          "coc.preferences.formatOnSaveFiletypes" = [ "*" ];
        };
      };

      package = recursiveUpdate pkgs.unstable.neovim-unwrapped {
        # Used by home-manager, which still assumes nvim 0.7.
        lua.pkgs.lib = pkgs.unstable.neovim-unwrapped.lua.pkgs.luaLib;
      };

      plugins = with pkgs.vimPlugins; [
        auto-pairs
        coc-eslint
        coc-json
        coc-prettier
        coc-tsserver
        copilot-vim
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

        # 3rd party
        alternaut-vim
        navitron-nvim
        teleport-vim
        unison-vim
        personal-vim-config

        # Treesitter integrations.
        nvim-treesitter-textobjects
        nvim-treesitter.withAllGrammars
      ];

      extraPackages = with pkgs.unstable; [
        nixfmt
        nodejs-16_x
        rustup
        shellcheck
        terraform
        vim-vint
      ];

      extraConfig = ''
        set shell=${pkgs.zsh}/bin/zsh
        source ${../../../../config/neovim.lua}
      '';
    };
  };
}
