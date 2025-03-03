{
  config,
  lib,
  pkgs,
  ...
}:

let
  cfg = config.psychollama.profiles.full;
in

{
  options.psychollama.profiles.full = {
    enable = lib.mkEnableOption "Create an opinionated editor";
  };

  config = lib.mkIf cfg.enable {
    package = pkgs.unstable.neovim;
    lsp.enable = lib.mkDefault true;

    psychollama.presets = {
      lsp.servers = {
        clangd.enable = lib.mkDefault true;
        gopls.enable = lib.mkDefault true;
        jsonls.enable = lib.mkDefault true;
        luals.enable = lib.mkDefault true;
        nil.enable = lib.mkDefault true;
        nushell.enable = lib.mkDefault true;
        rust-analyzer.enable = lib.mkDefault true;
        typescript.enable = lib.mkDefault true;
      };

      plugins = {
        alternaut-nvim.enable = lib.mkDefault true;
        avante-nvim.enable = lib.mkDefault true;
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
        img-clip-nvim.enable = lib.mkDefault config.psychollama.presets.plugins.avante-nvim.enable;
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

    settings = {
      # Editing settings
      backspace = [
        "indent"
        "eol"
        "start"
      ];
      formatoptions = "qc1orj";
      fileformat = "unix";
      fileformats = [
        "unix"
        "dos"
        "mac"
      ];
      textwidth = 78;
      expandtab = true;
      tabstop = 2;
      shiftwidth = 0;
      shiftround = true;

      # Interaction settings
      wildmenu = true;
      wildmode = [
        "longest"
        "list"
        "full"
      ];
      inccommand = "nosplit";
      wrapscan = false;
      pumheight = 10;
      pumblend = 20;
      winblend = 0;
      autoread = true;
      ignorecase = true;
      smartcase = true;
      mouse = "";
      completeopt = [
        "menu"
        "menuone"
        "noselect"
      ];

      # Display settings
      incsearch = true;
      showcmd = true;
      termguicolors = true;
      signcolumn = "yes";
      number = true;
      numberwidth = 3;
      list = true;
      listchars = "tab:) ,trail:.";
      foldenable = false;
      updatetime = 100;
      linebreak = true;
      cursorline = true;

      # Storage settings
      backupcopy = "yes";
      backup = true;
      backupdir = "/tmp";
      undofile = true;
      history = 10000;

      # Integrations
      clipboard = "unnamedplus";
      grepprg = "rg --vimgrep";
    };

    # TODO: Convert parts of the neovim config to Nix.
    extraConfig = ''
      vim.o.shell = "${pkgs.dash}/bin/dash"
      vim.cmd.source('${./neovim.lua}')
    '';
  };
}
