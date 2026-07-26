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
  config = {
    "${self.presets.lsp.servers.clangd}".enable = mkDefault true;
    "${self.presets.lsp.servers.jsonls}".enable = mkDefault true;
    "${self.presets.lsp.servers.luals}".enable = mkDefault true;
    "${self.presets.lsp.servers.nil}".enable = mkDefault true;
    "${self.presets.lsp.servers.nushell}".enable = mkDefault true;
    "${self.presets.lsp.servers.rust-analyzer}".enable = mkDefault true;
    "${self.presets.lsp.servers.typescript}".enable = mkDefault true;

    "${self.presets.plugins.alternaut-nvim}".enable = mkDefault true;
    "${self.presets.plugins.cmp-buffer}".enable = mkDefault true;
    "${self.presets.plugins.cmp-cmdline}".enable = mkDefault true;
    "${self.presets.plugins.cmp-nvim-lsp}".enable = mkDefault true;
    "${self.presets.plugins.cmp-path}".enable = mkDefault true;
    "${self.presets.plugins.conform-nvim}".enable = mkDefault true;
    "${self.presets.plugins.deja-view-nvim}".enable = mkDefault true;
    "${self.presets.plugins.dotfiles-nvim}".enable = mkDefault true;
    "${self.presets.plugins.fzf-vim}".enable = mkDefault true;
    "${self.presets.plugins.gitlinker-nvim}".enable = mkDefault true;
    "${self.presets.plugins.gitsigns-nvim}".enable = mkDefault true;
    "${self.presets.plugins.gutenberg-nvim}".enable = mkDefault true;
    "${self.presets.plugins.lualine-nvim}".enable = mkDefault true;
    "${self.presets.plugins.markdown-preview-nvim}".enable = mkDefault true;
    "${self.presets.plugins.navitron-nvim}".enable = mkDefault true;
    "${self.presets.plugins.note-nvim}".enable = mkDefault true;
    "${self.presets.plugins.nvim-autopairs}".enable = mkDefault true;
    "${self.presets.plugins.nvim-cmp}".enable = mkDefault true;
    "${self.presets.plugins.nvim-lint}".enable = mkDefault true;
    "${self.presets.plugins.onedarkpro-nvim}".enable = mkDefault true;
    "${self.presets.plugins.snacks-nvim}".enable = mkDefault true;
    "${self.presets.plugins.teleport-vim}".enable = mkDefault true;
    "${self.presets.plugins.telescope-fzf-native-nvim}".enable = mkDefault true;
    "${self.presets.plugins.telescope-nvim}".enable = mkDefault true;
    "${self.presets.plugins.treesj}".enable = mkDefault true;
    "${self.presets.plugins.vim-endwise}".enable = mkDefault true;
    "${self.presets.plugins.vim-fugitive}".enable = mkDefault true;
    "${self.presets.plugins.vim-repeat}".enable = mkDefault true;
    "${self.presets.plugins.vim-surround}".enable = mkDefault true;

    "${self.presets.plugins.nvim-treesitter}" = {
      enable = mkDefault true;

      # Bundle every grammar. `withAllGrammars` keeps the `nvim-treesitter`
      # pname, so this stays a single manifest entry rather than shadowing
      # a grammar-less copy installed separately.
      package = pkgs.unstable.vimPlugins.nvim-treesitter.withAllGrammars;
    };

    "${self.presets.plugins.nvim-treesitter-textobjects}" = {
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
