{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.profiles.full;
in

{
  options.profiles.full.enable = lib.mkEnableOption "Create an opinionated editor";

  config = lib.mkIf cfg.enable {
    package = pkgs.unstable.neovim;
    lsp.enable = lib.mkDefault true;

    presets = {
      lsp.servers = {
        clangd.enable = lib.mkDefault true;
        jsonls.enable = lib.mkDefault true;
        luals.enable = lib.mkDefault true;
        nil.enable = lib.mkDefault true;
        nushell.enable = lib.mkDefault true;
        rust-analyzer.enable = lib.mkDefault true;
        typescript.enable = lib.mkDefault true;
      };

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

    extraPlugins = [
      (pkgs.unstable.vimPlugins.nvim-treesitter.withPlugins (
        _:
        pkgs.unstable.vimPlugins.nvim-treesitter.allGrammars
        ++ [
          pkgs.tree-sitter.builtGrammars.tree-sitter-remix
          pkgs.tree-sitter.builtGrammars.tree-sitter-nu
        ]
      ))
    ];

    # TODO: Convert parts of the neovim config to Nix.
    extraConfig = ''
      vim.o.shell = "${pkgs.dash}/bin/dash"
      vim.cmd.source('${./neovim.lua}')
    '';
  };
}
