{ config, lib, pkgs, ... }:

with lib;

let
  trueByDefault = mkDefault true;
  cfg = config.presets.base;
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
    };
  };

in {
  options.presets.base.enable = mkEnableOption "Create an opinionated editor";

  config = mkIf cfg.enable {
    package = pkgs.unstable.neovim;

    plugins = {
      coc-nvim = {
        enable = trueByDefault;

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

            nix = {
              command = "${nil}/bin/nil";
              filetypes = [ "nix" ];
              rootPatterns = [ "flake.nix" ];
              settings.nil.formatting.command =
                [ "${nixfmt}/bin/nixfmt" "--quiet" ];
            };

            efm = {
              command = "${efm-langserver}/bin/efm-langserver";
              args = [ "-c" (yaml.generate "efm-config.yaml" efm-config) ];
              filetypes = attrNames efm-config.languages;
            };
          };

          "coc.preferences.formatOnSave" = true;
          "diagnostic.virtualText" = true;
          "diagnostic.virtualTextCurrentLineOnly" = false;
          "eslint.autoFixOnSave" = true;
          "outline.autoPreview" = true;
          "suggest.noselect" = true;
          "tsserver.useLocalTsdk" = true;
        };
      };

      coc-eslint.enable = trueByDefault;
      coc-json.enable = trueByDefault;
      coc-pairs.enable = trueByDefault;
      coc-prettier.enable = trueByDefault;
      coc-tsserver.enable = trueByDefault;
      copilot-vim.enable = trueByDefault;
      fzf-vim.enable = trueByDefault;
      lualine-nvim.enable = trueByDefault;
      markdown-preview-nvim.enable = trueByDefault;
      onedarkpro-nvim.enable = trueByDefault;
      splitjoin-vim.enable = trueByDefault;
      undotree.enable = trueByDefault;
      vader-vim.enable = trueByDefault;
      vim-commentary.enable = trueByDefault;
      vim-endwise.enable = trueByDefault;
      vim-fugitive.enable = trueByDefault;
      vim-gitgutter.enable = trueByDefault;
      vim-plug.enable = trueByDefault;
      vim-repeat.enable = trueByDefault;
      vim-surround.enable = trueByDefault;

      # 3rd party
      alternaut-vim.enable = trueByDefault;
      navitron-nvim.enable = trueByDefault;
      teleport-vim.enable = trueByDefault;
      unison-vim.enable = trueByDefault;
      personal-vim-config.enable = trueByDefault;

      # Treesitter integrations.
      nvim-treesitter-textobjects.enable = trueByDefault;
    };

    extraPlugins = with pkgs.vimPlugins; [ nvim-treesitter.withAllGrammars ];

    extraPackages = with pkgs.unstable; [
      nodejs-16_x
      rustup
      shellcheck
      terraform
      unzip # For source-diving Plug'n'Play dependencies.
      vim-vint
      yarn
    ];

    # TODO: Convert parts of the neovim config to Nix.
    extraConfig = ''
      set shell=${pkgs.zsh}/bin/zsh
      source ${../../../config/neovim.lua}
    '';
  };
}
