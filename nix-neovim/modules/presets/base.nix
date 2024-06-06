{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  trueByDefault = mkDefault true;
  efm = config.plugins.coc-nvim.efm;
  cfg = config.presets.base;

  # TODO: Pull this from `node_modules` instead.
  prettier = parser: {
    format-command = "${u.nodePackages.prettier}/bin/prettier --parser ${parser}";
    format-stdin = true;
  };

  # TODO: Pull this from `node_modules` instead.
  eslint = {
    lint-command = "${u.nodePackages.eslint}/bin/eslint --format visualstudio --stdin";
    lint-source = "eslint";
    lint-stdin = true;
    lint-ignore-exit-code = true;
    lint-formats = [
      "%f(%l,%c): %tarning %m"
      "%f(%l,%c): %rror %m"
    ];
  };

  u = pkgs.unstable;
in
{
  options.presets.base.enable = mkEnableOption "Create an opinionated editor";

  config = mkIf cfg.enable {
    package = pkgs.unstable.neovim;

    lsp = {
      enable = true;

      servers = {
        rust-analyzer = {
          command = [ "${u.rust-analyzer}/bin/rust-analyzer" ];
          filetypes = [ "rust" ];
          root.patterns = [ "Cargo.toml" ];
        };

        nil = {
          command = [ "${u.nil}/bin/nil" ];
          filetypes = [ "nix" ];
          root.patterns = [ "flake.nix" ];
          settings.nil = {
            nix.flake.autoArchive = true;
            formatting.command = [
              "${u.nixfmt-rfc-style}/bin/nixfmt"
              "--quiet"
            ];
          };
        };

        nushell = {
          command = [
            "nu"
            "--lsp"
          ];

          filetypes = [ "nu" ];
          root.patterns = [ ".git/" ];
        };

        lua-language-server = {
          command = [ "${u.lua-language-server}/bin/lua-language-server" ];
          filetypes = [ "lua" ];
          root.patterns = [
            ".git/"
            "lua/"
          ];
        };

        efm-langserver = {
          command = [
            "${u.efm-langserver}/bin/efm-langserver"
            "-c"
            efm.configFile
          ];

          filetypes = efm.filetypes;
          root.patterns = [ ".git/" ];
        };

        typescript-language-server = {
          command = [
            "${u.nodePackages.typescript-language-server}/bin/typescript-language-server"
            "--stdio"
          ];

          filetypes = [
            "typescript"
            "typescriptreact"
            "javascript"
            "javascriptreact"
          ];

          root.patterns = [
            "package.json"
            "tsconfig.json"
            ".git/"
          ];
        };

        jsonls = {
          command = [
            "${u.nodePackages.vscode-json-languageserver-bin}/bin/json-languageserver"
            "--stdio"
          ];

          filetypes = [
            "json"
            "jsonc"
            "json5"
          ];

          root.patterns = [ ".git/" ];
        };
      };
    };

    plugins = {
      coc-nvim = {
        enable = false;

        efm = {
          enable = false; # TODO: Finish migrating to native LSP.

          settings.languages = rec {
            vim = {
              lint-command = "${u.vim-vint}/bin/vint -";
              lint-source = "vint";
              lint-stdin = true;
              lint-formats = [ "%f:%l:%c: %m" ];
            };

            bash = {
              lint-command = "${u.shellcheck}/bin/shellcheck -f gcc -x -";
              lint-source = "shellcheck";
              lint-stdin = true;
              lint-formats = [
                "%f:%l:%c: %trror: %m"
                "%f:%l:%c: %tarning: %m"
                "%f:%l:%c: %tote: %m"
              ];
            };

            sh = bash;

            typescript = (prettier "typescript") // eslint;

            javascript = typescript;
            javascriptreact = typescript;
            typescriptreact = typescript;

            css = prettier "css";
            graphql = prettier "graphql";
            html = prettier "html";
            json = prettier "json";
            json5 = prettier "json5";
            jsonc = prettier "jsonc";
            less = prettier "less";
            markdown = prettier "markdown";
            vue = prettier "vue";
            yaml = prettier "yaml";
          };
        };
      };

      markdown-preview-nvim = {
        enable = trueByDefault;
        browser = mkDefault "firefox";
      };

      cmp-buffer.enable = trueByDefault;
      cmp-nvim-lsp.enable = trueByDefault;
      cmp-path.enable = trueByDefault;
      copilot-chat-nvim.enable = trueByDefault;
      copilot-vim.enable = trueByDefault;
      fzf-vim.enable = trueByDefault;
      gitlinker-nvim.enable = trueByDefault;
      gitsigns-nvim.enable = trueByDefault;
      lualine-nvim.enable = trueByDefault;
      nvim-autopairs.enable = trueByDefault;
      nvim-cmp.enable = trueByDefault;
      nvim-luapad.enable = trueByDefault;
      onedarkpro-nvim.enable = trueByDefault;
      telescope-nvim.enable = trueByDefault;
      telescope-undo-nvim.enable = trueByDefault;
      treesj.enable = trueByDefault;
      unison.enable = trueByDefault;
      vader-vim.enable = trueByDefault;
      vim-endwise.enable = trueByDefault;
      vim-fugitive.enable = trueByDefault;
      vim-plug.enable = trueByDefault;
      vim-repeat.enable = trueByDefault;
      vim-surround.enable = trueByDefault;

      # 3rd party
      deja-view-vim.enable = trueByDefault;
      navitron-nvim.enable = trueByDefault;
      remix-nvim.enable = trueByDefault;
      teleport-vim.enable = trueByDefault;
      personal-vim-config.enable = trueByDefault;

      alternaut-vim = {
        enable = trueByDefault;

        patterns = rec {
          python = {
            file_naming_conventions = [
              "test_{name}.{ext}"
              "{name}.{ext}"
            ];
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
            file_naming_conventions = [
              "{name}.test.{ext}"
              "{name}.test.unit.{ext}"
            ];
            directory_naming_conventions = [ "__tests__" ];
            file_extensions = [
              "ts"
              "tsx"
              "js"
            ];
          };

          vader = vim;
          vim = {
            file_naming_conventions = [ "{name}.{ext}" ];
            directory_naming_conventions = [ "tests" ];
            file_extensions = [
              "vim"
              "vader"
            ];
          };
        };
      };

      # Treesitter integrations.
      nvim-treesitter-textobjects.enable = trueByDefault;
    };

    extraPlugins = with pkgs.vimPlugins; [
      (nvim-treesitter.withPlugins (
        _:
        nvim-treesitter.allGrammars
        ++ [
          pkgs.tree-sitter.builtGrammars.tree-sitter-remix
          pkgs.tree-sitter.builtGrammars.tree-sitter-nu
        ]
      ))
    ];

    extraPackages = with pkgs.unstable; [
      rustup
      unzip # For source-diving Plug'n'Play dependencies.
      yarn
    ];

    # TODO: Convert parts of the neovim config to Nix.
    extraConfig = ''
      set shell=${pkgs.dash}/bin/dash
      source ${../../../config/neovim.lua}
    '';
  };
}
