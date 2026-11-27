# Every plugin. One with settings of its own gets a directory beside this.
{
  units,
  self,
  define,
  ...
}:

let
  inherit (units) editor;
  inherit (units.editor) plugins;

  plugin = name: config: editor.plugin { inherit name config; };
in

{
  main = define.unit {
    provides = [
      (plugin "cmp-buffer" null)
      (plugin "cmp-cmdline" null)
      (plugin "cmp-nvim-lsp" null)
      (plugin "cmp-path" null)
      (plugin "deja-view-nvim" null)
      (plugin "dotfiles-nvim" null)
      (plugin "fzf-vim" null)
      (plugin "gitlinker-nvim" ./gitlinker.lua)
      (plugin "gutenberg-nvim" ./gutenberg.lua)

      # Defer gitsigns until a buffer is read (~18ms saved)
      (editor.plugin {
        name = "gitsigns-nvim";
        config = ./gitsigns.lua;
        defer.event = "BufReadPre";
      })

      (plugin "lualine-nvim" ./lualine.lua)

      # Defer the markdown preview until markdown files are opened. It runs a
      # Node.js server to render the preview.
      (editor.plugin {
        name = "markdown-preview-nvim";
        config = ./markdown-preview/config.lua;
        defer.ft = "markdown";
        nodejs = true;
      })

      (plugin "navitron-nvim" ./navitron.lua)

      # Configured through `opts`; its config forwards them to
      # `require('note').setup`.
      (editor.plugin {
        name = "note-nvim";
        config = ./note.lua;
        opts.path = "~/attic/slip-box";
      })

      (plugin "nvim-autopairs" ./autopairs.lua)
      (plugin "nvim-cmp" ./nvim-cmp.lua)

      # Bundle every grammar. `withAllGrammars` keeps the `nvim-treesitter`
      # pname, so this stays a single manifest entry rather than shadowing a
      # grammar-less copy installed separately.
      (editor.plugin {
        name = "nvim-treesitter";
        package = pkgs: pkgs.unstable.vimPlugins.nvim-treesitter.withAllGrammars;
      })

      # Nixpkgs tracks frozen master branch; main branch has nvim-treesitter compat fixes
      (editor.plugin {
        name = "nvim-treesitter-textobjects";
        config = ./nvim-treesitter-textobjects.lua;

        package =
          pkgs:
          pkgs.unstable.vimPlugins.nvim-treesitter-textobjects.overrideAttrs {
            doCheck = false;
            version = "0-unstable-2026-01-02";
            src = pkgs.fetchFromGitHub {
              owner = "nvim-treesitter";
              repo = "nvim-treesitter-textobjects";
              rev = "28a3494c075ef0f353314f627546537e43c09592";
              hash = "sha256-5VeIAW09my+4fqXbzVG7RnLXrjpXAk/g2vd7RbhNws8=";
            };
          };
      })

      (plugin "onedarkpro-nvim" ./onedarkpro.lua)
      (plugin "teleport-vim" ./teleport.lua)
      (plugin "telescope-fzf-native-nvim" ./telescope-fzf-native.lua)
      (plugin "telescope-nvim" ./telescope.lua)

      # Defer treesj until its keymap is used (~13ms saved)
      (editor.plugin {
        name = "treesj";
        config = ./treesj.lua;
        defer.keys = "<leader>j";
      })

      (plugin "vim-endwise" null)
      (plugin "vim-fugitive" null)
      (plugin "vim-repeat" null)
      (plugin "vim-surround" null)
    ];
  };

  unit = define.group {
    includes = [
      self.main
      plugins.alternaut.unit
      plugins.conform.unit
      plugins.nvim-lint.unit
      plugins.snacks.unit
    ];
  };
}
