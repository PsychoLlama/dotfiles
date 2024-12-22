{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.profiles.full;
  u = pkgs.unstable;
in

{
  options.profiles.full.enable = lib.mkEnableOption "Create an opinionated editor";

  config = lib.mkIf cfg.enable {
    package = pkgs.unstable.neovim;

    presets = {
      plugins = {
        alternaut-nvim.enable = lib.mkDefault true;
        cmp-buffer.enable = lib.mkDefault true;
        cmp-cmdline.enable = lib.mkDefault true;
        cmp-nvim-lsp.enable = lib.mkDefault true;
        cmp-path.enable = lib.mkDefault true;
        codecompanion-nvim.enable = lib.mkDefault true;
        conform-nvim.enable = lib.mkDefault true;
        copilot-vim.enable = lib.mkDefault true;
        deja-view-vim.enable = lib.mkDefault true;
        dressing-nvim.enable = lib.mkDefault true;
        ext-nvim.enable = lib.mkDefault true;
        fzf-vim.enable = lib.mkDefault true;
        gitlinker-nvim.enable = lib.mkDefault true;
        gitsigns-nvim.enable = lib.mkDefault true;
        lualine-lsp-progress.enable = lib.mkDefault true;
        lualine-nvim.enable = lib.mkDefault true;
        markdown-preview-nvim.enable = lib.mkDefault true;
        navitron-nvim.enable = lib.mkDefault true;
        neotest-vitest.enable = lib.mkDefault true;
        neotest.enable = lib.mkDefault true;
        nvim-autopairs.enable = lib.mkDefault true;
        nvim-cmp.enable = lib.mkDefault true;
        nvim-lint.enable = lib.mkDefault true;
        nvim-luapad.enable = lib.mkDefault true;
        nvim-treesitter-textobjects.enable = lib.mkDefault true;
        nvim-treesitter.enable = lib.mkDefault true;
        onedarkpro-nvim.enable = lib.mkDefault true;
        remix-nvim.enable = lib.mkDefault true;
        teleport-vim.enable = lib.mkDefault true;
        telescope-fzf-native-nvim.enable = lib.mkDefault true;
        telescope-nvim.enable = lib.mkDefault true;
        telescope-undo-nvim.enable = lib.mkDefault true;
        treesj.enable = lib.mkDefault true;
        vim-endwise.enable = lib.mkDefault true;
        vim-fugitive.enable = lib.mkDefault true;
        vim-repeat.enable = lib.mkDefault true;
        vim-surround.enable = lib.mkDefault true;
      };
    };

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
            ".luarc.json"
          ];

          # Reference: https://luals.github.io/wiki/settings/
          settings.Lua = {
            # Using stylua instead.
            format.enable = false;

            # Don't try to dynamically manage library type defs.
            workspace.checkThirdParty = false;
            addonManager.enable = false;
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
      vim.cmd.source('${./neovim.lua}')
    '';
  };
}
