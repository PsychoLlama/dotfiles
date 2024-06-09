{
  config,
  lib,
  pkgs,
  ...
}:

let
  enabled = lib.mkDefault true;
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
  options.presets.base.enable = lib.mkEnableOption "Create an opinionated editor";

  config = lib.mkIf cfg.enable {
    package = pkgs.unstable.neovim;

    lsp = {
      enable = true;

      servers = {
        rust-analyzer = {
          server = "${u.rust-analyzer}/bin/rust-analyzer";
          filetypes = [ "rust" ];
          root.patterns = [ "Cargo.toml" ];
        };

        nil = {
          server = "${u.nil}/bin/nil";
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
          server = "nu";
          args = [ "--lsp" ];
          filetypes = [ "nu" ];
          root.patterns = [ ".git/" ];
        };

        lua-language-server = {
          server = "${u.lua-language-server}/bin/lua-language-server";
          filetypes = [ "lua" ];
          root.patterns = [
            ".git/"
            "lua/"
          ];
        };

        typescript-language-server = {
          server = "${u.nodePackages.typescript-language-server}/bin/typescript-language-server";
          args = [ "--stdio" ];
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
          server = "${u.nodePackages.vscode-json-languageserver-bin}/bin/json-languageserver";
          args = [ "--stdio" ];
          filetypes = [
            "json"
            "jsonc"
            "json5"
          ];

          root.patterns = [ ".git/" ];
        };
      };

      efm = {
        enable = true;
        settings.languages = rec {
          vim = {
            lint-command = "${u.vim-vint}/bin/vint -";
            lint-source = "vint";
            lint-stdin = true;
            lint-formats = [ "%f:%l:%c: %m" ];
          };

          sh = bash;
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

    plugins = {
      markdown-preview-nvim = {
        enable = enabled;
        browser = lib.mkDefault "firefox";
      };

      copilot-chat-nvim = {
        enable = enabled;
        extraConfig = ''
          require('CopilotChat').setup({
            -- Default options
          })
        '';
      };

      telescope-nvim = {
        enable = enabled;
        extraConfig = ./plugins/telescope.lua;
      };

      telescope-undo-nvim = {
        enable = enabled;
        extraConfig = ''
          require('telescope').load_extension('undo')
        '';
      };

      nvim-treesitter = {
        enable = enabled;
        extraConfig = ./plugins/nvim-treesitter.lua;
      };

      gitlinker-nvim = {
        enable = enabled;
        extraConfig = ./plugins/gitlinker.lua;
      };

      gitsigns-nvim = {
        enable = enabled;
        extraConfig = ./plugins/gitsigns.lua;
      };

      treesj = {
        enable = enabled;
        extraConfig = ./plugins/treesj.lua;
      };

      nvim-cmp = {
        enable = enabled;
        extraConfig = ./plugins/nvim-cmp.lua;
      };

      nvim-autopairs = {
        enable = enabled;
        extraConfig = ''
          require('nvim-autopairs').setup({
            check_ts = true,
          })
        '';
      };

      copilot-vim = {
        enable = enabled;
        extraConfig = ''
          vim.g.copilot_no_tab_map = true
          vim.api.nvim_set_keymap('i', '<c-j>', 'copilot#Accept("\\<CR>")', { noremap = true, expr = true, silent = true })
        '';
      };

      lualine-nvim = {
        enable = enabled;
        extraConfig = ./plugins/lualine.lua;
      };

      cmp-buffer.enable = enabled;
      cmp-nvim-lsp.enable = enabled;
      cmp-path.enable = enabled;
      fzf-vim.enable = enabled;
      nvim-luapad.enable = enabled;
      onedarkpro-nvim.enable = enabled;
      unison.enable = enabled;
      vader-vim.enable = enabled;
      vim-endwise.enable = enabled;
      vim-fugitive.enable = enabled;
      vim-plug.enable = enabled;
      vim-repeat.enable = enabled;
      vim-surround.enable = enabled;

      # 3rd party
      navitron-nvim = {
        enable = enabled;
        extraConfig = ''
          require('navitron').setup({
            -- Default options
          })
        '';
      };

      deja-view-vim.enable = enabled;
      remix-nvim.enable = enabled;
      teleport-vim.enable = enabled;
      personal-vim-config.enable = enabled;

      alternaut-vim = {
        enable = enabled;

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
      nvim-treesitter-textobjects.enable = enabled;
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
      vim.o.shell = "${pkgs.dash}/bin/dash"
      vim.cmd.source('${../../../config/neovim.lua}')
    '';
  };
}
