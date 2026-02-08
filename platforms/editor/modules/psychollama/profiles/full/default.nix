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
        vtsls.enable = lib.mkDefault true;
        vue.enable = lib.mkDefault true;
      };

      plugins = {
        alternaut-nvim.enable = lib.mkDefault true;
        cmp-buffer.enable = lib.mkDefault true;
        cmp-cmdline.enable = lib.mkDefault true;
        cmp-nvim-lsp.enable = lib.mkDefault true;
        cmp-path.enable = lib.mkDefault true;
        conform-nvim.enable = lib.mkDefault true;
        deja-view-vim.enable = lib.mkDefault true;
        fzf-vim.enable = lib.mkDefault true;
        gitlinker-nvim.enable = lib.mkDefault true;
        gitsigns-nvim.enable = lib.mkDefault true;
        lab-nvim.enable = lib.mkDefault true;
        lualine-lsp-progress.enable = lib.mkDefault true;
        lualine-nvim.enable = lib.mkDefault true;
        markdown-nvim.enable = lib.mkDefault true;
        markdown-preview-nvim.enable = lib.mkDefault true;
        navitron-nvim.enable = lib.mkDefault true;
        nvim-autopairs.enable = lib.mkDefault true;
        nvim-cmp.enable = lib.mkDefault true;
        nvim-lint.enable = lib.mkDefault true;
        nvim-luapad.enable = lib.mkDefault true;
        nvim-treesitter-textobjects = {
          enable = lib.mkDefault true;

          # Nixpkgs tracks frozen master branch; main branch has nvim-treesitter compat fixes
          package = pkgs.unstable.vimPlugins.nvim-treesitter-textobjects.overrideAttrs {
            doCheck = false;
            version = "0-unstable-2026-01-02";
            src = pkgs.fetchFromGitHub {
              owner = "nvim-treesitter";
              repo = "nvim-treesitter-textobjects";
              rev = "28a3494c075ef0f353314f627546537e43c09592";
              hash = "sha256-5VeIAW09my+4fqXbzVG7RnLXrjpXAk/g2vd7RbhNws8=";
            };
          };
        };
        nvim-treesitter.enable = lib.mkDefault true;
        onedarkpro-nvim.enable = lib.mkDefault true;
        snacks-nvim.enable = lib.mkDefault true;
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
      pkgs.unstable.vimPlugins.nvim-treesitter.withAllGrammars
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
      undofile = true;
      history = 10000;

      # Integrations
      clipboard = "unnamedplus";
      grepprg = "rg --vimgrep";
    };

    # TODO: Convert parts of the neovim config to Nix.
    extraConfig = ''
      -- Set mapleader before loading plugins so deferred keymaps expand correctly.
      vim.g.mapleader = ' '
      vim.api.nvim_set_keymap('n', '<space>', '<nop>', {})
      vim.o.shell = "${pkgs.dash}/bin/dash"
      vim.cmd.source('${./neovim.lua}')
    '';
  };
}
