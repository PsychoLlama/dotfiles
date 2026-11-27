# Everything I want in the editor. Stands without a machine around it:
# `nix shell dotfiles#editor`.
{
  units,
  self,
  define,
  ...
}:

let
  inherit (units) editor;
in

{
  main = define.unit {
    config = [
      (editor.neovim (
        pkgs:
        if pkgs.stdenv.hostPlatform.isLinux then pkgs.unstable.custom.nvim-rs else pkgs.unstable.neovim
      ))

      (editor.config (pkgs: ''
        -- Set mapleader before loading plugins so deferred keymaps expand correctly.
        vim.g.mapleader = ' '
        vim.api.nvim_set_keymap('n', '<space>', '<nop>', {})
        vim.o.shell = "${pkgs.dash}/bin/dash"
        vim.cmd.source('${./neovim.lua}')
      ''))
    ];
  };

  unit = define.group {
    config = [
      self.main
      editor.lsp.clangd.unit
      editor.lsp.jsonls.unit
      editor.lsp.luals.unit
      editor.lsp.nil.unit
      editor.lsp.nushell.unit
      editor.lsp.rust-analyzer.unit
      editor.lsp.taplo.unit
      editor.lsp.typescript.unit
      editor.plugins.unit
    ];
  };
}
