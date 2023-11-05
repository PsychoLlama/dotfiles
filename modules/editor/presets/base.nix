{ config, lib, pkgs, ... }:

with lib;

let
  trueByDefault = mkDefault true;
  cfg = config.presets.base;

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

            nix = {
              command = "${nil}/bin/nil";
              filetypes = [ "nix" ];
              rootPatterns = [ "flake.nix" ];
              settings.nil.formatting.command =
                [ "${nixfmt}/bin/nixfmt" "--quiet" ];
            };
          };

          "coc.preferences.formatOnSave" = true;
          "diagnostic.virtualText" = true;
          "diagnostic.virtualTextCurrentLineOnly" = false;
          "eslint.autoFixOnSave" = true;
          "outline.autoPreview" = true;
          "suggest.noselect" = true;
          "tsserver.useLocalTsdk" = true;
          "sumneko-lua.serverDir" = lua-language-server;
          "Lua.diagnostics.globals" = [ "vim" ];
        };

        efm = with pkgs.unstable; {
          enable = trueByDefault;

          settings.languages = rec {
            vim = {
              lint-command = "${vim-vint}/bin/vint -";
              lint-source = "vint";
              lint-stdin = true;
              lint-formats = [ "%f:%l:%c: %m" ];
            };

            bash = {
              lint-command = "${shellcheck}/bin/shellcheck -f gcc -x -";
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
      };

      markdown-preview-nvim = {
        enable = trueByDefault;
        browser = mkDefault "firefox";
      };

      coc-eslint.enable = trueByDefault;
      coc-json.enable = trueByDefault;
      coc-pairs.enable = trueByDefault;
      coc-prettier.enable = trueByDefault;
      coc-sumneko-lua.enable = trueByDefault;
      coc-tsserver.enable = trueByDefault;
      copilot-vim.enable = trueByDefault;
      fzf-vim.enable = trueByDefault;
      lualine-nvim.enable = trueByDefault;
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
      deja-view-vim.enable = trueByDefault;
      navitron-nvim.enable = trueByDefault;
      teleport-vim.enable = trueByDefault;
      unison-vim.enable = trueByDefault;
      personal-vim-config.enable = trueByDefault;

      alternaut-vim = {
        enable = trueByDefault;

        patterns = rec {
          python = {
            file_naming_conventions = [ "test_{name}.{ext}" "{name}.{ext}" ];
            directory_naming_conventions = [ "tests" ];
            file_extensions = [ "py" ];
          };

          "javascript.jsx" = javascript;
          javascript = {
            file_naming_conventions = [ "{name}.test.{ext}" ];
            directory_naming_conventions = [ "__tests__" ];
            file_extensions = [ "js" ];
          };

          "typescript.tsx" = typescript;
          "typescriptreact" = typescript;
          typescript = {
            file_naming_conventions =
              [ "{name}.test.{ext}" "{name}.test.unit.{ext}" ];
            directory_naming_conventions = [ "__tests__" ];
            file_extensions = [ "ts" "tsx" "js" ];
          };

          vader = vim;
          vim = {
            file_naming_conventions = [ "{name}.{ext}" ];
            directory_naming_conventions = [ "tests" ];
            file_extensions = [ "vim" "vader" ];
          };
        };
      };

      # Treesitter integrations.
      nvim-treesitter-textobjects.enable = trueByDefault;
    };

    extraPlugins = with pkgs.vimPlugins; [ nvim-treesitter.withAllGrammars ];

    extraPackages = with pkgs.unstable; [
      nodejs-18_x
      rustup
      unzip # For source-diving Plug'n'Play dependencies.
      yarn
    ];

    # TODO: Convert parts of the neovim config to Nix.
    extraConfig = ''
      set shell=${pkgs.zsh}/bin/zsh
      source ${../../../config/neovim.lua}
    '';
  };
}
