{
  imports = [
    ../../lsp/clangd.nix
    ../../lsp/jsonls.nix
    ../../lsp/luals.nix
    ../../lsp/nil.nix
    ../../lsp/nushell.nix
    ../../lsp/rust-analyzer.nix
    ../../lsp/typescript.nix
    ../../plugins/default.nix
    ../../trusted-directories.nix
  ];

  exports.editor =
    { lib, pkgs, ... }:

    {
      package = if pkgs.stdenv.hostPlatform.isLinux then pkgs.custom.nvim-rs else pkgs.unstable.neovim;

      lsp.enable = lib.mkDefault true;

      # note.nvim is configured through the manifest opts; its config hook
      # (`aspects/editor/plugins/note.lua`) forwards these to `require('note').setup`.
      plugins.note-nvim.opts.path = lib.mkDefault "~/attic/slip-box";

      psychollama.presets.plugins = {
        # Nixpkgs tracks frozen master branch; main branch has nvim-treesitter compat fixes
        nvim-treesitter-textobjects.package =
          pkgs.unstable.vimPlugins.nvim-treesitter-textobjects.overrideAttrs
            {
              doCheck = false;
              version = "0-unstable-2026-01-02";
              src = pkgs.fetchFromGitHub {
                owner = "nvim-treesitter";
                repo = "nvim-treesitter-textobjects";
                rev = "28a3494c075ef0f353314f627546537e43c09592";
                hash = "sha256-5VeIAW09my+4fqXbzVG7RnLXrjpXAk/g2vd7RbhNws8=";
              };
            };

        # Bundle every grammar. `withAllGrammars` keeps the `nvim-treesitter`
        # pname, so this stays a single manifest entry rather than shadowing
        # a grammar-less copy installed separately.
        nvim-treesitter.package = pkgs.unstable.vimPlugins.nvim-treesitter.withAllGrammars;
      };

      extraConfig = ''
        -- Set mapleader before loading plugins so deferred keymaps expand correctly.
        vim.g.mapleader = ' '
        vim.api.nvim_set_keymap('n', '<space>', '<nop>', {})
        vim.o.shell = "${pkgs.dash}/bin/dash"
        vim.cmd.source('${./neovim.lua}')
      '';
    };
}
