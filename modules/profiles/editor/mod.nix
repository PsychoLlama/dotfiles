{
  self,
  lib,
  pkgs,
  ...
}:

# An opinionated neovim: the plugin set, language servers and vimrc that turn
# the editor framework into something usable. Enabled by
# `presets.programs.editor` on a host, and by the `packages.editor` flake
# output for a portable copy.

let
  inherit (lib) mkDefault;
in

{
  config."${self}".presets = {
    lsp.servers = {
      clangd.enable = mkDefault true;
      jsonls.enable = mkDefault true;
      luals.enable = mkDefault true;
      nil.enable = mkDefault true;
      nushell.enable = mkDefault true;
      rust-analyzer.enable = mkDefault true;
      typescript.enable = mkDefault true;
    };

    plugins = {
      alternaut-nvim.enable = mkDefault true;
      cmp-buffer.enable = mkDefault true;
      cmp-cmdline.enable = mkDefault true;
      cmp-nvim-lsp.enable = mkDefault true;
      cmp-path.enable = mkDefault true;
      conform-nvim.enable = mkDefault true;
      deja-view-nvim.enable = mkDefault true;
      dotfiles-nvim.enable = mkDefault true;
      fzf-vim.enable = mkDefault true;
      gitlinker-nvim.enable = mkDefault true;
      gitsigns-nvim.enable = mkDefault true;
      gutenberg-nvim.enable = mkDefault true;
      lualine-nvim.enable = mkDefault true;
      markdown-preview-nvim.enable = mkDefault true;
      navitron-nvim.enable = mkDefault true;
      note-nvim.enable = mkDefault true;
      nvim-autopairs.enable = mkDefault true;
      nvim-cmp.enable = mkDefault true;
      nvim-lint.enable = mkDefault true;
      onedarkpro-nvim.enable = mkDefault true;
      snacks-nvim.enable = mkDefault true;
      teleport-vim.enable = mkDefault true;
      telescope-fzf-native-nvim.enable = mkDefault true;
      telescope-nvim.enable = mkDefault true;
      treesj.enable = mkDefault true;
      vim-endwise.enable = mkDefault true;
      vim-fugitive.enable = mkDefault true;
      vim-repeat.enable = mkDefault true;
      vim-surround.enable = mkDefault true;

      nvim-treesitter = {
        enable = mkDefault true;

        # Bundle every grammar. `withAllGrammars` keeps the `nvim-treesitter`
        # pname, so this stays a single manifest entry rather than shadowing
        # a grammar-less copy installed separately.
        package = pkgs.unstable.vimPlugins.nvim-treesitter.withAllGrammars;
      };

      nvim-treesitter-textobjects = {
        enable = mkDefault true;

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
    };
  };

  platforms.editor = {
    package = if pkgs.stdenv.hostPlatform.isLinux then pkgs.custom.nvim-rs else pkgs.unstable.neovim;

    lsp.enable = mkDefault true;

    # note.nvim is configured through the manifest opts; its config hook
    # (presets/plugins/note-nvim/config.lua) forwards these to
    # `require('note').setup`.
    plugins.note-nvim.opts.path = mkDefault "~/attic/slip-box";

    extraConfig = ''
      -- Set mapleader before loading plugins so deferred keymaps expand correctly.
      vim.g.mapleader = ' '
      vim.api.nvim_set_keymap('n', '<space>', '<nop>', {})
      vim.o.shell = "${pkgs.dash}/bin/dash"
      vim.cmd.source('${./neovim.lua}')
    '';
  };
}
