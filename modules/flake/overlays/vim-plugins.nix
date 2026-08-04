{ withSystem, ... }:

{
  flake.overlays.vim-plugins =
    final: prev:

    withSystem prev.stdenv.hostPlatform.system (
      { inputs', ... }:

      {
        # Custom vim plugins live under `pkgs.custom.vimPlugins` rather than being
        # merged into `pkgs.vimPlugins`. The editor platform merges them back into the
        # by-name lookup via its `vimPlugins` option, so presets keep referencing them
        # by name without polluting the upstream set.
        custom = (prev.custom or { }) // {
          vimPlugins = {
            "dotfiles-nvim" = prev.callPackage ../../../pkgs/dotfiles.nvim { };
            "note-nvim" = prev.callPackage ../../../pkgs/note.nvim { };
            "alternaut-nvim" = inputs'.alternaut-nvim.packages.default;
            "deja-view-nvim" = inputs'.deja-view-nvim.packages.default;
            "gutenberg-nvim" = inputs'.gutenberg-nvim.packages.default;
            "navitron-nvim" = inputs'.navitron-nvim.packages.default;
            "teleport-vim" = inputs'.teleport-vim.packages.default;
          };
        };
      }
    );
}
