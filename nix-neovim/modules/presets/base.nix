{
  config,
  lib,
  pkgs,
  ...
}:

let
  enabled = lib.mkDefault true;
  cfg = config.presets.base;
  u = pkgs.unstable;

  eslint = {
    lint-command = "${u.eslint_d}/bin/eslint_d --format visualstudio --stdin --stdin-filename \"\${INPUT}\"";
    lint-source = "eslint";
    lint-stdin = true;
    lint-formats = [
      "%f(%l,%c): %tarning %m"
      "%f(%l,%c): %rror %m"
    ];
  };
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
          settings.nil.nix.flake.autoArchive = true;
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

          # Reference: https://luals.github.io/wiki/settings/
          settings.Lua = {
            workspace.library = [
              # Provides type definitions for `vim.*`.
              "${config.package}/share/nvim/runtime/lua"
            ];
          };
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
            "tsconfig.json"
            ".git/"
          ];
        };

        jsonls = {
          server = "${u.nodePackages.vscode-langservers-extracted}/bin/vscode-json-language-server";
          args = [ "--stdio" ];
          filetypes = [
            "json"
            "jsonc"
            "json5"
          ];

          root.patterns = [ ".git/" ];
        };

        clangd = {
          # Uses `pkgs.clang-tools` on NixOS and XCode/Visual Studio on other
          # platforms.
          server = "clangd";

          filetypes = [
            "c"
            "cpp"
            "objc"
            "objcpp"
          ];

          root.patterns = [ "compile_commands.json" ];
        };
      };

      efm = {
        enable = true;
        settings.languages = rec {
          vim = [
            {
              lint-command = "${u.vim-vint}/bin/vint -";
              lint-source = "vint";
              lint-stdin = true;
              lint-formats = [ "%f:%l:%c: %m" ];
            }
          ];

          sh = bash;
          bash = [
            {
              lint-command = "${u.shellcheck}/bin/shellcheck -f gcc -x -";
              lint-source = "shellcheck";
              lint-stdin = true;
              lint-formats = [
                "%f:%l:%c: %trror: %m"
                "%f:%l:%c: %tarning: %m"
                "%f:%l:%c: %tote: %m"
              ];
            }
          ];

          javascript = typescript;
          javascriptreact = typescript;
          typescriptreact = typescript;
          typescript = [ eslint ];
        };
      };
    };

    plugins = {
      markdown-preview-nvim = {
        enable = enabled;
        extraConfig = ./plugins/markdown-preview.lua;
        opts.browser = lib.mkDefault "firefox";
      };

      codecompanion-nvim = {
        enable = enabled;
        extraConfig = ./plugins/codecompanion.lua;
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

      dressing-nvim = {
        enable = enabled;
        extraConfig = ''
          require('dressing').setup({
            input = {
              relative = "editor",
            },
          })
        '';
      };

      navitron-nvim = {
        enable = enabled;
        extraConfig = ./plugins/navitron.lua;
      };

      teleport-vim = {
        enable = enabled;
        extraConfig = ''
          vim.g['teleport#update_cwd'] = true
        '';
      };

      alternaut-vim = {
        enable = enabled;
        extraConfig = ./plugins/alternaut.lua;
        opts = rec {
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
              "{name}.unit.{ext}"
            ];
            directory_naming_conventions = [ "__tests__" ];
            file_extensions = [
              "ts"
              "tsx"
              "js"
              "jsx"
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

      onedarkpro-nvim = {
        enable = enabled;
        extraConfig = ./plugins/onedarkpro.lua;
      };

      conform-nvim = {
        enable = enabled;
        extraConfig = ./plugins/conform.lua;
        opts = {
          format_on_save = true;
          default_format_opts.undojoin = true;

          formatters = {
            nixfmt = {
              command = "${u.nixfmt-rfc-style}/bin/nixfmt";
              args = [ "--quiet" ];
              stdin = true;
            };

            prettier = {
              command = "${u.prettierd}/bin/prettierd";
              args = [ "$FILENAME" ];
              stdin = true;
            };

            eslint = {
              command = "${u.eslint_d}/bin/eslint_d";
              stdin = true;
              args = [
                "--stdin-filename"
                "$FILENAME"
                "--stdin"
                "--fix-to-stdout"
              ];
            };
          };

          formatters_by_ft = rec {
            javascript = typescript;
            javascriptreact = typescript;
            typescriptreact = typescript;
            typescript = [
              "eslint"
              "prettier"
            ];

            css = [ "prettier" ];
            graphql = [ "prettier" ];
            html = [ "prettier" ];
            json = [ "prettier" ];
            json5 = [ "prettier" ];
            jsonc = [ "prettier" ];
            less = [ "prettier" ];
            markdown = [ "prettier" ];
            nix = [ "nixfmt" ];
            vue = [ "prettier" ];
            yaml = [ "prettier" ];
          };
        };
      };

      cmp-buffer.enable = enabled;
      cmp-cmdline.enable = enabled;
      cmp-nvim-lsp.enable = enabled;
      cmp-path.enable = enabled;
      deja-view-vim.enable = enabled;
      ext-nvim.enable = enabled;
      fzf-vim.enable = enabled;
      nvim-luapad.enable = enabled;
      nvim-treesitter-textobjects.enable = enabled;
      remix-nvim.enable = enabled;
      vim-endwise.enable = enabled;
      vim-fugitive.enable = enabled;
      vim-repeat.enable = enabled;
      vim-surround.enable = enabled;
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

    extraPackages =
      with pkgs.unstable;
      [
        rustup
        unzip # For source-diving Plug'n'Play dependencies.
        yarn
      ]
      ++ lib.optionals pkgs.stdenv.isLinux [ pkgs.unstable.clang-tools ];

    # TODO: Convert parts of the neovim config to Nix.
    extraConfig = ''
      vim.o.shell = "${pkgs.dash}/bin/dash"
      vim.cmd.source('${../../../config/neovim.lua}')
    '';
  };
}
